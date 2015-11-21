       library(httr)
       set_config(
         use_proxy(url="proxy.enerweb.co.za", port=3128, username="booysej",password="rofdeer")
       )
       
       options(RCurlOptions = list(proxy="proxy.enerweb.co.za:3128", proxyuserpwd="booysej:rofdeer"))