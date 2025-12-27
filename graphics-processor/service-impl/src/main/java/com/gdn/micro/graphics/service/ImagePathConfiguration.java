package com.gdn.micro.graphics.service;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;

@Service
public class ImagePathConfiguration {

  @Value("${image.path.configuration.mapping}")
  private String imagePathConfigurationMapping;

  public String getLocationPrefix(String clientKey) {
    String locationPrefix = getImageConfigurationPathMapping().get(clientKey);
    if (locationPrefix == null || locationPrefix.trim().length() == 0) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION);
    }
    return locationPrefix;
  }

  private Map<String, String> getImageConfigurationPathMapping() {
    return Stream.of(imagePathConfigurationMapping.split("\\,"))
        .collect(Collectors.toMap(t -> t.split("=")[0].trim(), t -> t.split("=")[1].trim()));
  }
}