Currently the script `automate.R` is running as cron job under Julien's account.

To access the cron tab

```{bash}
crontab -u brun -e
```

The different settings are being used to run the script on Tue and Thu at 10:55PM PST

```{bash}
55 22 * * 1,4 /usr/local/bin/Rscript /home/brun/github_com/SNAPP/soc-twitter/automate.R "/home/brun/github_com/SNAPP/soc-twitter/" >> log_soc_cron.log
```

