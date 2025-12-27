package com.gdn.partners.product.analytics.web;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.data.mongo.MongoRepositoriesAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.test.context.ContextConfiguration;

import com.gdn.partners.product.analytics.properties.ClientParameterProperties;
import com.gdn.partners.product.analytics.web.configuration.TestConfiguration;

/**
 * @author Pradeep Reddy
 */
@SpringBootApplication(exclude = {
    MongoAutoConfiguration.class,
    MongoDataAutoConfiguration.class,
    MongoRepositoriesAutoConfiguration.class})
@EnableConfigurationProperties({ClientParameterProperties.class})
@ContextConfiguration(classes={TestConfiguration.class})
public class TestApplication {

}
