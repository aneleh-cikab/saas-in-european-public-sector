# PARSING DATA TO CSV

# for html file in folder data/tender-data-htmls read html file in soup using bs4

import glob
import bs4
import csv

# CHANGE PATHS HERE
path_to_html = "C:/Thesis-data/*.html"
path_to_html_trimmed = "C:/Thesis-data\\"   

files=glob.glob(path_to_html) 

for number,file in enumerate(files, 1):

    if number % 1000 == 0:
        print("Now at number: ", number)
    
            
    html = open(file, 'r', encoding="utf-8")
    soup = bs4.BeautifulSoup(html, 'html.parser')
    id = file.replace(path_to_html_trimmed, '').replace('.html', '')
    
    # gtting elements from the soup and saving them as text
    try:
        title_1 = soup.select_one('span:-soup-contains("Title")').find_next_sibling("div").text.strip()
    except:
        title_1 = None
    try:
        title_2 = soup.select_one('span:-soup-contains("Title attributed to the contract")').find_next_sibling("div").text.strip()
    except:
        title_2 = None
    title = title_1 if title_1 else title_2

    try:
        cpv_1 = soup.select_one('span:-soup-contains("Main CPV code")').find_next_sibling("div").text.strip()
    except:
        cpv_1 = None
    try:
        cpv_2 = soup.select_one('span:-soup-contains("Common procurement vocabulary (CPV)")').find_next_sibling("div").text.strip()
    except:
        cpv_2 = None
    cpv = cpv_1 if cpv_1 else cpv_2

    
    try:
        contract_type = soup.select_one('span:-soup-contains("Type of contract")').find_next_sibling("div").text.strip()
    except:
        contract_type = None
    
    try:
        description_1 = soup.select_one('span:-soup-contains("Short description")').find_next_sibling("div").text.strip()
    except:
        description_1 = None
    try:
        description_2 = soup.select_one('span:-soup-contains("Short description of the contract or purchase(s)")').find_next_sibling("div").text.strip()
    except:
        description_2 = None
    try:
        description_3 = soup.select_one('span:-soup-contains("Description of the procurement")').find_next_sibling("div").text.strip()
    except:
        description_3 = None     
    
    if description_1:
        description = description_1
    elif description_2:
        description = description_2
    else:
        description = description_3
    
    try:
        country_1 = soup.select_one('span:-soup-contains("Name and addresses")').find_next_sibling("div")
    except:
        country_1 = None
    try:
        country_2 = soup.select_one('span:-soup-contains("addresses")').find_next_sibling("div")
    except:
        country_2 = None
    country = country_1 if country_1 else country_2
    try:
        contracting_authority_type = soup.select_one('span:-soup-contains("Type of the contracting authority")').find_next_sibling("div").text.strip()
    except:
        contracting_authority_type = None
    try:
        contracting_authority_activity = soup.select_one('span:-soup-contains("Main activity")').find_next_sibling("div").text.strip()
    except:
        contracting_authority_activity = None
    
    # Tenders can have estimated price with two different titles. I have tried to make this part of the code nicer, but did not find a way, so I am running it twice.

    try:
        value_1 = soup.select_one('span:-soup-contains("Estimated total value")').find_next_sibling("div").text.strip()
    except:
        value_1 = None
    try:
        value_2 = soup.select_one('span:-soup-contains("Total value of the procurement (excluding VAT)")').find_next_sibling("div").text.strip()
    except:
        value_2 = None
    try:
        value_3 = soup.select_one('span:-soup-contains("Total final value of contract")').find_next_sibling("div").text.strip()
    except:
        value_3 = None
    try:
        value_4 = soup.select_one('span:-soup-contains("Total quantity or scope")').find_next_sibling("div").text.strip()
    except:
        value_4 = None
    try:
        value_5 = soup.select_one('span:-soup-contains("II.2.1)")').find_next_sibling("div").text.strip()
    except:
        value_5 = None
    
    if value_1:
        value = value_1
    elif value_2:
        value = value_2
    elif value_3:
        value = value_3
    elif value_4:
        value = value_4
    else:
        value = value_5
    
    try:
        type_of_procedure = soup.select_one('span:-soup-contains("IV.1.1)")').find_next_sibling("div").text.strip()
    except:
        type_of_procedure = None
    try:
        government_procurement_agreement = soup.select_one('span:-soup-contains("Government Procurement Agreement (GPA)")').find_next_sibling("div").text.strip()
    except:
        government_procurement_agreement = None
    try:
        framework_agreement_1 = soup.select_one('span:-soup-contains("Information about a framework agreement or a dynamic purchasing system")').find_next_sibling("div").text.strip()
    except:
        framework_agreement_1 = None
    try:
        framework_agreement_2 = soup.select_one('span:-soup-contains("II.1.3)")').find_next_sibling("div").text.strip()
    except:
        framework_agreement_2 = None
    framework_agreement = framework_agreement_1 if framework_agreement_1 else framework_agreement_2
    
    try:
        notice_dispatch_date = soup.select_one('span:-soup-contains("Date of dispatch of this notice:")').find_next_sibling("div").text.strip()
    except:
        notice_dispatch_date = None
    try:
        application_date = soup.select_one('span:-soup-contains("Time limit for receipt of tenders or requests to participate")').find_next_sibling("div").text.strip()
    except:
        application_date = None
    try:
        tender_opening_date = soup.select_one('span:-soup-contains("Conditions for opening of tenders")').find_next_sibling("div").text.strip()
    except:
        tender_opening_date = None

    # saving data
    with open('../data/parsed_data_new.csv', 'a', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow([id, title, cpv, contract_type, description, country, contracting_authority_type, contracting_authority_activity, value, type_of_procedure, 
                 government_procurement_agreement, framework_agreement, notice_dispatch_date, application_date, tender_opening_date])