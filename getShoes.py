from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import urllib2
import csv
import time
import re

url = "https://stockx.com/search?s=jordan"
driver = webdriver.Chrome()
driver.implicitly_wait(30)
driver.get(url)
driver.implicitly_wait(30)

shoe_dict = {}

### Scroll to bottom of page
SCROLL_PAUSE_TIME = 3

# Get scroll height
last_height = driver.execute_script("return document.body.scrollHeight")

while True:
    # Scroll down to bottom
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

    # Wait to load page
    time.sleep(SCROLL_PAUSE_TIME)

    # Calculate new scroll height and compare with last scroll height
    new_height = driver.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break
    last_height = new_height


shoes = driver.find_elements_by_class_name("result-tile")
for s in shoes:
	shoe_link = s.find_element_by_tag_name('a')
	shoe_url = shoe_link.get_attribute('href').encode('ascii', 'ignore')
	shoe_name = s.find_element_by_tag_name("h3").text.encode('ascii', 'ignore').replace("\n", " ")
	shoe_dict[shoe_name] = shoe_url

shoes = []
exclude = ['Style ', 'Colorway ', 'Retail Price ', 'Release Date ']

for shoe, link in shoe_dict.items():
	print shoe
	shoe_row = [shoe]
	page = urllib2.urlopen(link)
	soup = BeautifulSoup(page, 'lxml')

	historical = []
	try:
		pastyear_stats = soup.find_all("div", class_ = "gauge-container")
		for i in pastyear_stats:
			val = i.find("div", class_ = "gauge-value")
			if val is not None:
				historical.append(val.text)
			else:
				historical.append('--')

	except:
		historical = ['--']*3

	shoe_row.extend(historical)

	try:
		release_date = soup.find("div", class_ = "product-details detail-row").text
		price_range = soup.find("div", class_="ds-range value-container").text
		
		for e in exclude:
			release_date = release_date.replace(e, '')

		colorway = re.search('(?<=\d\s).+(?=\s\$)\s', release_date)
		if colorway is not None:
			colorway = colorway.group()
		else: 
			colorway = '--'
		release_date = release_date.replace(colorway, '').strip()
		colorway = colorway.rstrip()
		shoe_row.extend(release_date.split(' '))
		shoe_row.append(colorway)
		shoe_row.append(price_range)
		shoes.append(shoe_row)
			
		
	except:
		rd = soup.find("div", class_ = "product-details detail-column")
		pr = price_range = soup.find("div", class_="ds-range value-container")
		if rd is not None:
			release_date = rd.text
			for e in exclude:
				release_date = release_date.replace(e, '').strip()
			colorway = re.search('(?<=\d\s).+(?=\s\$)\s', release_date)
			if colorway is not None:
				colorway = colorway.group()
			else:
				colorway = '--'
			release_date = release_date.replace(colorway, '').strip()
			release_date = release_date.split(' ')
			colorway = colorway.rstrip()
		else:
			release_date = ['--']*4
		if pr is not None:
			price_range = pr.text
		else:
			price_range = None
		shoe_row.extend(release_date)
		shoe_row.append(colorway)
		shoe_row.append(price_range)
		shoes.append(shoe_row)


with open("shoeDataV2.csv", 'w') as f:
	writer = csv.writer(f)
	writer.writerows(shoes)


