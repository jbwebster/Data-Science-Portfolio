#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 27 14:09:52 2020

@author: jacebwebster
"""

# Modules
from bs4 import BeautifulSoup
#import urllib2
import urllib.request
import csv
import argparse
import json
import random
import time
import re
import sys



def generateMainURL(zipcode, page_number):
    """
    Create URL corresponding to initial search page results,
    given a zipcode and the page of results desired
    """
    first_segment_url = "https://www.cars.com/for-sale/searchresults.action/?dealerType=all&page="
    second_segment_url = "&perPage=100&rd=20&searchSource=PAGINATION&sort=relevance&zc="
    extended_url = first_segment_url + str(page_number) + second_segment_url + str(zipcode)
    return extended_url



def generateVehicleURLs(vehiclePaths):
    """
    Given relative paths to vehicle detail pages from the search result page,
    return a list of absolute paths to the pages (urls)
    """
    urls = []
    for i in range(0, len(vehiclePaths)):
        urls.append("https://www.cars.com" + vehiclePaths[i])
    return urls
    


def saveResponse(webpage, i, zipcode):
    """
    Save an html file
    """
    filename = str(zipcode) + "_" + str(i) + "_response.html" 
    with open(filename, 'w') as f:
        f.write(webpage)
        

def getNewUsed(soup):
    """
    Given a vehicle page, return whether the vehicle is new or used.
    Returns False if the page isn't formatted as expected.
    """
    try:
        res = soup.find_all("h1", class_="vehicle-info__stock-type")
        new_used = res[0].text.strip()
        if new_used == "NEW" or new_used == "USED":
            return new_used
        else:
            return False
    except:
        return False

# Use html info to get year/make/model of the car
def getYearMakeModel(soup):
    """
    Given a vehicle page, extract the year, make and model info.
    Return empty strings if not formatted as expected.
    """
    try:
        res = soup.find_all("h1", class_="cui-heading-2--secondary vehicle-info__title")
        year_make_model = res[0].text.strip()
        year_make_model = year_make_model.split(" ")
        year = year_make_model[0]
        # Problematic for makes that have multiple words
        # Land Rover is the only make I know of that is two words,
        # so handle that special case
        make = year_make_model[1]
        if make == "Land":
            make = make + " " + "Rover"
        s = " "
        if make == "Land Rover":
            model = s.join(year_make_model[3:])
        else:
            model = s.join(year_make_model[2:])
        return year, make, model
    except:
        return "", "", ""

# Use html info to get the MSRP. 
def getMSRP(soup):
    """
    Given a vehicle page, extract the msrp of the vehicle.
    Remove dollar signs and commas from the msrp.
    Return empty string if not formatted as expected.
    """
    try:
        res = soup.find("div", class_="vehicle-info__price--msrp")
        pattern = "\$[0-9,]*"
        msrp = re.findall(pattern, res[0].text)[0]
        # Remove "$" and "," from the msrp, for downstream analysis
        msrp = msrp[0].replace("$", "")
        msrp = msrp.replace(",", "")
        if msrp is None:
            return ""
        else:
            return msrp
    except:
        return ""

def getDealerPrice(soup):
    """
    Given a vehicle page, extract the sale price listed by the dealer.
    Remove dollar signs and commas from the price.
    Return empty string if not formatted as expected.
    """
    try:
        res = soup.find_all("span", class_="vehicle-info__price-display vehicle-info__price-display--dealer cui-heading-2")
        dealer_price = res[0].text.split()
        dealer_price = dealer_price[0].replace("$", "")
        dealer_price = dealer_price.replace(",", "")   
        return dealer_price
    except:
        return ""
    

def getBasicInfo(soup):
    """
    Given a vehicle page, extract the information listed under the 'Basic'
    section of the page: Fuel type, exterior color, city mpg, interior 
    color, highway mpg, drivetain, transmission, liters (engine), V (engine),
    and mileage.
    Return empty strings for variables that aren't able to be extracted.
    Returned as a list.
    """
    info = ["", "", "", \
            "", "", "", \
            "", "", "", ""]
    
    info_types = ["Fuel Type", "Exterior Color", "City MPG", \
                  "Interior Color", "Highway MPG", "Drivetrain", \
                  "Transmission", "Liters", "V", "Mileage"]
    
    res = soup.find_all("li", class_="vdp-details-basics__item")
    # Extract data
    for i in range(0, len(res)):
        try:
            info_list = res[i].text.strip().split("\n")
            info_list = [i.strip() for i in info_list if i.strip()]
            info_list = info_list[0].split(":")
            info_list[1] = info_list[1].strip()
            if info_list[0] == "Engine":
                liters = re.findall("[\d\.]*L", info_list[1])[0]
                liters = liters.replace("L", "")
                V = re.search("(V[\d]*)|([\d]V)", info_list[1])[0]
                V = V.replace("V", "")
                info[7] = liters
                info[8] = V
            elif info_list[0] == "Mileage":
                info[9] = info_list[1].replace(",", "")
                if info_list[1] == "Not provided":
                    info[9] = ""

            elif info_list[0] == "Highway MPG":
                info[4] = re.findall("[\d]*", info_list[1])[0]
            elif info_list[0] == "City MPG":
                info[2] = re.findall("[\d]*", info_list[1])[0]
            else:
                index = info_types.index(info_list[0])
                info[index] = info_list[1]
        except:
            continue
    return info
        
def getOverallReview(soup):
    """
    Given a vehicle page, extract the overall customer review (out of 5)
    of the vehicle.
    Returns empty string if not formatted as expected.
    """
    try:
        res = soup.find_all("div", class_="overall-review-stars")
        review = re.findall("[\d\.]*", str(res[0].text))
        review = [i.strip() for i in review if i.strip()]
        return review[0]
    except:
        return ""

def parseVehicleData(soup, zipcode, page, url):
    """
    Driver function for extraction all data that the scraper is interested
    in from a vehicle's page.
    Returns a list.
    """
    # Functions here return "NA" is they fail to extract the data
    new_used = getNewUsed(soup)
    if new_used == False:
        return False
    year, make, model = getYearMakeModel(soup)
    msrp = getMSRP(soup)
    dealer_price = getDealerPrice(soup)
    # Format: [Fuel, Ext. Color, CityMPG, Int. Color, HighwayMPG,
    #           Drivetrain, Transmission, Liters (engine), V (engine), mileage]
    basic_info = getBasicInfo(soup)
    overallReview = getOverallReview(soup)
    vehicle_info = [zipcode, page, new_used, year, make, model, \
                    msrp, dealer_price, basic_info[0], \
                    basic_info[1], basic_info[2], basic_info[3], \
                    basic_info[4], basic_info[5], basic_info[6], \
                    basic_info[7], basic_info[8], basic_info[9], \
                    overallReview, url]
    
    for i in range(0, len(vehicle_info)):
        if vehicle_info[i] is None:
            vehicle_info[i] = ""
    return vehicle_info

  
def getVehiclePaths(soup):
    """
    Given a search results page, return a list of the relative paths
    from the page to the vehicle detail pages linked to from
    the search results page.
    """
    vehiclePaths = []
    res = soup.find_all("a", class_="shop-srp-listings__listing")
    for a in res:
        vehiclePaths.append(a['href'])
    return vehiclePaths    

def exportResults(zipcode, data, isHeader):
    """
    Save an individual row to our outfile (csv format). 
    """
    outfile = "multi_car_data_" + str(zipcode) + ".csv"
    if isHeader:
        with open(outfile, mode='w') as f:
            writer = csv.writer(f, delimiter=',', quotechar='"')
            writer.writerow(data)
        f.close()
    else:
        with open(outfile, mode='a') as f:
            writer = csv.writer(f, delimiter=',', quotechar='"')
            writer.writerow(data)
        f.close()
    print("Finished Export")


def scrape(zipcode):
    """
    Main driver function for the web scraper.
    """

        
    # For each zipcode, scrape 85% of the vehicles on the 
    # first 5 pages of search results
    for zipcode in zipcodes:
        # Create the header line of the output csv
        results = ["Zipcode", "Page", "NewUsed", "Year", "Make", "Model", \
                    "MSRP", "Dealer_Price", "Fuel_Type", \
                    "Exterior_Color", "City_MPG", \
                    "Interior_Color", "Highway_MPG", \
                    "Drivetrain", "Transmission", \
                    "Liters", "V", "Mileage", "Customer_Review", "URL"]
        exportResults(zipcode, results, True)
        print("Zipcode: " + str(zipcode))
        for page in range(1,5):
        
            print("Page: " + str(page))
            # Stall, to avoid making too many calls to the webpage,
            # so I don't get blocked 
            rand_time = random.randint(1, 10)
            time.sleep(30 + rand_time)
            mainURL = generateMainURL(zipcode, page)
            content = urllib.request.urlopen(mainURL)

            try:
                print("Making soup of search page results..")
                soup = BeautifulSoup(content, features = "lxml")
                print("Extracting vehicle paths..")
                paths = getVehiclePaths(soup)
                #if len(paths) == 0:
                #    print("No paths, access denied..")
                #    print(str(zipcode) + " " + str(page))
                #    sys.exit()
                
                print("Building individual vehicle URLs..")
                vehicleURLs = generateVehicleURLs(paths)
                print("Number of vehicle urls: " + str(len(vehicleURLs)))
                # Shuffle the urls, to reduce chances of getting blocked
                random.shuffle(vehicleURLs)
                for url in vehicleURLs:
                    # Stall, to avoid making too many calls to the webpage,
                    # so I don't get blocked. (1 minute - 3 minutes)
                    rand_time = random.randint(45, 180)
                    time.sleep(rand_time)
                    # Only check ~90% of the cars, to help avoid looking like
                    # a web crawler
                    rand_prob = random.randint(0, 100)
                    if rand_prob <= 90:
                        print("Retrieving: " + url)
                        try:
                            print("Getting content and creating soup...")
                            content = urllib.request.urlopen(url)
                            soup = BeautifulSoup(content, features="lxml")
                            print("Parsing vehicle data..")
                            vehicle_information = parseVehicleData(soup, zipcode, page, url)
                            print(vehicle_information)
                            if vehicle_information == False:
                                raise ValueError
                            print("Saving to results file..")
                            exportResults(zipcode, vehicle_information, False)
                        except:
                            print("ERROR")
                            continue
            except:
                print("ERROR2")
                continue
        
 


def getZipcodes(zipcode):
    """
    Read input file and return a list of zipcodes.
    """
    zipcodes = []
    with open(zipcode, mode='r') as f:
        for line in f:
            zipcodes.append(line.strip())   
    return zipcodes


if __name__ == "__main__":
    # Create argument parser
    argparser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter)
    argparser.add_argument("-z", "--zipcode", help="Zipcode")
    argparser.add_argument("-o", "--out", help="Outfile Name")
    # Parse arguments
    args = argparser.parse_args()
    zipcode = args.zipcode
    #outfile = args.out
    # Get list of zipcodes to scrape
    zipcodes = getZipcodes(zipcode)
    # Scrape the site

    scrape(zipcodes)