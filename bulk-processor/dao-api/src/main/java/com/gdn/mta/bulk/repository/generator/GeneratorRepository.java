package com.gdn.mta.bulk.repository.generator;

import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;

public interface GeneratorRepository {
  
  String DEFAULT_USERNAME = "system";

  GenerateShippingWeightResponse generateShippingWeight(GenerateShippingWeightRequest request) throws Exception;

}
