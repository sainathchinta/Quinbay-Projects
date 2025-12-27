package com.gdn.partners.pcu.external.web;

import com.gdn.partners.pcu.external.properties.GCSProperties;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.test.context.ContextConfiguration;

import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import com.gdn.partners.pcu.external.properties.ClientParameterProperties;
import com.gdn.partners.pcu.external.properties.EstimatedPriceProperties;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.properties.QRCodeProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.web.config.TestConfiguration;

/**
 * @author Pradeep Reddy
 */
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class,  HibernateJpaAutoConfiguration.class})
@EnableConfigurationProperties({ClientParameterProperties.class, ApplicationProperties.class,
    EstimatedPriceProperties.class, ImageProperties.class, SystemParameterProperties.class,
  QRCodeProperties.class, GCSProperties.class})
@ContextConfiguration(classes={TestConfiguration.class})
public class TestApplication {

}
