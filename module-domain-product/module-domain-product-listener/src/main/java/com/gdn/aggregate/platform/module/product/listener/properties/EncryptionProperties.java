package com.gdn.aggregate.platform.module.product.listener.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Configuration
@ConfigurationProperties("encryption")
public class EncryptionProperties {

  private String password;

  private String salt;

}
