package com.gdn.partners.pcu.internal.web;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.test.context.ContextConfiguration;

import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.web.config.TestConfiguration;

/**
 * @author Pradeep Reddy
 */
@SpringBootApplication
@EnableConfigurationProperties({ClientParameterProperties.class, ImageProperties.class})
@ContextConfiguration(classes={TestConfiguration.class})
public class TestApplication {

}
