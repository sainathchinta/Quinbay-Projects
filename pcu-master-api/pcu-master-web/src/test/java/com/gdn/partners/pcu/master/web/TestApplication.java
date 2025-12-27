package com.gdn.partners.pcu.master.web;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.test.context.ContextConfiguration;

import com.gdn.partners.pcu.master.properties.ClientParameterProperties;
import com.gdn.partners.pcu.master.web.config.TestConfiguration;

/**
 * @author Pradeep Reddy
 */
@SpringBootApplication(exclude = DataSourceAutoConfiguration.class)
@EnableConfigurationProperties({ClientParameterProperties.class})
@ContextConfiguration(classes={TestConfiguration.class})
public class TestApplication {

}
