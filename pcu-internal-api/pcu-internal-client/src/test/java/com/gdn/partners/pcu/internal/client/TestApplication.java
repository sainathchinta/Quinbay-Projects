package com.gdn.partners.pcu.internal.client;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.netflix.hystrix.EnableHystrix;
import org.springframework.cloud.openfeign.EnableFeignClients;

import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

/**
 * @author Pradeep Reddy
 */
@EnableHystrix
@EnableFeignClients
@SpringBootApplication(exclude = HibernateJpaAutoConfiguration.class)
@EnableConfigurationProperties({
    ApplicationProperties.class
})
public class TestApplication {

}
