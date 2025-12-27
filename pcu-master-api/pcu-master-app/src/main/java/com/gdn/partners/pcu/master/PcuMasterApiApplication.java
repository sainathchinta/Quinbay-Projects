package com.gdn.partners.pcu.master;

/**
 * @author Pradeep Reddy
 */
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.openfeign.EnableFeignClients;

import com.gdn.partners.pcu.master.properties.ApplicationProperties;
import com.gdn.partners.pcu.master.properties.ClientParameterProperties;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@EnableFeignClients
@SpringBootApplication(scanBasePackages = {"com.gdn.partners.pcu.master"})
@EnableConfigurationProperties({
    ClientParameterProperties.class,
    ApplicationProperties.class
})
@EnableWebMvc
public class PcuMasterApiApplication {

  public static void main(String[] args) {
    SpringApplication.run(PcuMasterApiApplication.class, args);
  }

}
