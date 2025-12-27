package com.gdn.x.productcategorybase.controller.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class MapperUtilImpl implements MapperUtil {

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public List<ShippingResponse> getShippingResponseList(List<CategoryShipping> categoryShippings) throws Exception {
    List<ShippingResponse> shippingResponses = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(categoryShippings)) {
      categoryShippings.stream().map(categoryShipping -> this.getShippingResponse(categoryShipping))
          .collect(Collectors.toCollection(() -> shippingResponses));
    }
    return shippingResponses;
  }

  private ShippingResponse getShippingResponse(CategoryShipping categoryShipping) {
    ShippingResponse shippingResponse = new ShippingResponse();
    try {
      shippingResponse = this.mapStringToShippingCodeResponse(categoryShipping.getShippingCode());
      BeanUtils.copyProperties(categoryShipping, shippingResponse);
    } catch (Exception e) {
      log.error("Error in converting shippingCode to shippingResponse", e);
    }
    return shippingResponse;
  }

  @Override
  public String mapRequestToString(Object request) throws Exception {
    return this.objectMapper.writeValueAsString(request);
  }

  @Override
  public ShippingResponse mapStringToShippingCodeResponse(String content) throws Exception {
    return this.objectMapper.readValue(content, ShippingResponse.class);
  }
}
