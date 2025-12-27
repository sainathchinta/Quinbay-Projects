package com.gdn.partners.pcu.internal;

/**
 * @author Pradeep Reddy
 */
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.kafka.annotation.EnableKafka;

import com.gdn.partners.pcu.internal.properties.ApplicationProperties;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import com.gdn.partners.pcu.internal.properties.ExtCatalogProperties;
import com.gdn.partners.pcu.internal.properties.GCSProperties;
import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;

@EnableKafka
@EnableFeignClients
@SpringBootApplication(exclude = HibernateJpaAutoConfiguration.class)
@EnableConfigurationProperties({ClientParameterProperties.class, ApplicationProperties.class,
    ExtCatalogProperties.class, ImageProperties.class, SystemParameterProperties.class, GCSProperties.class,
    GCSProperties.class})
public class PcuInternalApiApplication {

  public static void main(String[] args) {
      SpringApplication.run(PcuInternalApiApplication.class, args);
  }

}
