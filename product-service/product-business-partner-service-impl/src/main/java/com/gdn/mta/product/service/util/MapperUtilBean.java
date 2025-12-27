package com.gdn.mta.product.service.util;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;

@Service
public class MapperUtilBean implements MapperUtil {

  @Autowired
  private ObjectMapper mapper;

  @Override
  public String mapRequestToString(Object request) throws Exception {
    return mapper.writeValueAsString(request);
  }

  @Override
  public List<ProductItemWholesalePriceResponse> mapStringToResponse(String wholesaleRules) throws IOException {
    return mapper.readValue(wholesaleRules, new TypeReference<List<ProductItemWholesalePriceResponse>>() {
    });
  }
}
