package com.gdn.partners.pcu.external;

/**
 * @author Pradeep Reddy
 */
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import com.gdn.partners.pcu.external.properties.ClientParameterProperties;
import com.gdn.partners.pcu.external.properties.EstimatedPriceProperties;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.properties.QRCodeProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.netflix.hystrix.EnableHystrix;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@EnableHystrix
@EnableFeignClients
@EnableWebMvc
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class, MongoAutoConfiguration.class,
    HibernateJpaAutoConfiguration.class})
@EnableConfigurationProperties({
    ClientParameterProperties.class,
    ApplicationProperties.class,
    ImageProperties.class,
    EstimatedPriceProperties.class,
    SystemParameterProperties.class,
    QRCodeProperties.class,
    GCSProperties.class
})
public class PcuExternalApiApplication {

  public static void main(String[] args) {
    SpringApplication.run(PcuExternalApiApplication.class, args);
  }

}
