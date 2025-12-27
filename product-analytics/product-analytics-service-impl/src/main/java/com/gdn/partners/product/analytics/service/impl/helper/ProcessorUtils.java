package com.gdn.partners.product.analytics.service.impl.helper;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.model.enums.ProductAttributeExtractionsStatus;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProcessorUtils {

  public static <T> List<T> getDataListFromJsonFile(String filePath, Class<T> clazz, ObjectMapper objectMapper)
      throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
      return reader.lines().map(line -> getDetailFromJson(objectMapper, line, clazz)).filter(Objects::nonNull)
          .collect(Collectors.toList());
    }
  }

  public static List<ProductAttributeExtractions> getDataProductAttributeExtractionsListFromJsonFile(
      String filePath, ObjectMapper objectMapper) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
      return reader.lines()
          .map(line -> getDetailFromJson(objectMapper, line, ProductAttributeExtractions.class))
          .filter(Objects::nonNull).peek(
              productAttributeExtractions -> productAttributeExtractions.setStatus(
                  ProductAttributeExtractionsStatus.NEW.name())).collect(Collectors.toList());
    }
  }

  private static <T> T getDetailFromJson(ObjectMapper objectMapper, String line, Class<T> clazz) {
    try {
      return objectMapper.readValue(line, clazz);
    } catch (IOException e) {
      log.error("Couldn't convert JSON to AutoQCDetail for : {}", line);
      return null;
    }
  }
}
