FROM sscc-base/rshiny4
COPY model /opt/R-apps/cadmium-pig
RUN echo 'local({options(shiny.port = 3838, shiny.host = "0.0.0.0")})' >> /opt/R/4.0.5/lib/R/etc/Rprofile.site
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/opt/R-apps/cadmium-pig')"]