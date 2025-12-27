package com.gdn.mta.bulk.repository.generator;

import java.util.UUID;

import com.gdn.mta.bulk.feignConfig.PBPFeign;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;

@Repository
public class GeneratorRepositoryBean implements GeneratorRepository {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public GenerateShippingWeightResponse generateShippingWeight(GenerateShippingWeightRequest request) throws Exception {
    String requestId =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID().toString()
            : GdnMandatoryRequestParameterUtil.getRequestId();
    String username =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? GeneratorRepository.DEFAULT_USERNAME
            : GdnMandatoryRequestParameterUtil.getUsername();
    GdnRestSingleResponse<GenerateShippingWeightResponse> response =
        this.pbpFeign.generateShippingWeight(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), requestId, username, request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getValue();
  }
}
