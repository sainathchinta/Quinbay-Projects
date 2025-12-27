package com.gdn.mta.product.service.util;

import java.io.IOException;
import java.util.List;

import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;

public interface MapperUtil {

  /**
   * Convert json object to string
   *
   * @param request
   * @return
   * @throws Exception
   */
  String mapRequestToString(Object request) throws Exception;

  /**
   * Convert string to json object
   *
   * @param wholesaleRules
   * @return
   * @throws IOException
   */
  List<ProductItemWholesalePriceResponse> mapStringToResponse(String wholesaleRules) throws IOException;

}
