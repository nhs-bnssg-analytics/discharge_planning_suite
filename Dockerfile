FROM ubuntu2204_r422_dpp

RUN apt update && apt -y install cron

# Copy the train models script
COPY utils.R /root/utils.R
COPY code_combine.R /root/code_combine.R
COPY theme.R /root/theme.R
COPY colour_functions.R /root/colour_functions.R
COPY utils.R /root/utils.R

# Copy hello-cron file to the cron.d directory
COPY crontab /etc/cron.d/crontab

# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/crontab

# Apply cron job
RUN crontab /etc/cron.d/crontab

# Run the command on container startup
CMD printenv > /etc/environment && cron -f