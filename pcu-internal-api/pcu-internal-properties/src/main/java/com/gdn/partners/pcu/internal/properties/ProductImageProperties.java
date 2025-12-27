package com.gdn.partners.pcu.internal.properties;

import com.gdn.partners.pcu.internal.model.Constants;
import lombok.Data;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

@Data
@Configuration
@ConfigurationProperties(value = "product.image")
public class ProductImageProperties implements InitializingBean {
  private Map<String, String> validExtensionsToMagicNumbers;

  @Override
  public void afterPropertiesSet() {
    if (Objects.nonNull(validExtensionsToMagicNumbers)) {
      Map<String, String> correctedEntries = new HashMap<>();
      for (Map.Entry<String, String> entry : validExtensionsToMagicNumbers.entrySet()) {
        String key = entry.getKey();
        if (!key.startsWith(Constants.DOT)) {
          key = Constants.DOT + key;
        }
        correctedEntries.put(key, entry.getValue());
      }
      validExtensionsToMagicNumbers.clear();
      validExtensionsToMagicNumbers.putAll(correctedEntries);
    }
  }
}