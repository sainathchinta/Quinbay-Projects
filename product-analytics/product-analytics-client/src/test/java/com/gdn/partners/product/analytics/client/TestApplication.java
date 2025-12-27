package com.gdn.partners.product.analytics.client;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.openfeign.EnableFeignClients;

import com.gdn.partners.product.analytics.properties.ApplicationProperties;

/**
 * @author Pradeep Reddy
 */
@EnableFeignClients
@SpringBootApplication
@EnableConfigurationProperties({
    ApplicationProperties.class
})
public class TestApplication {

}
