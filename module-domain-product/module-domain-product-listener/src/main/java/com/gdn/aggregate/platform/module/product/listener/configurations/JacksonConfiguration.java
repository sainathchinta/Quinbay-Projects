package com.gdn.aggregate.platform.module.product.listener.configurations;

import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(prefix = Enabler.JACKSON_ENABLED, value="true")
public class JacksonConfiguration {

    /**
     * Enabling afterburner module for jackson
     * because we have performance problem
     * in AGP product module, related to big JSON
     * (de)serialization.
     *
     * Performance suggestion is from here:
     * https://github.com/FasterXML/jackson-docs/wiki/Presentation:-Jackson-Performance
     */
    @Bean
    public AfterburnerModule afterburnerModule() {
        return new AfterburnerModule();
    }

}
