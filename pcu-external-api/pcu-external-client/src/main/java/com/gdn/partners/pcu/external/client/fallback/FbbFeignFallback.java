package com.gdn.partners.pcu.external.client.fallback;


import com.blibli.oss.common.response.Response;
import com.blibli.oss.common.response.ResponseHelper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.fbb.core.constant.SortOrder;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.partners.pcu.external.client.feign.FbbFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class FbbFeignFallback implements FbbFeign{

  @Override
  public Response<List<ConsignmentStatusResponse>> partnerConsignmentFormsByFilterForProduct(
    String sortBy, SortOrder sortOrder, Integer page, Integer itemPerPage,
    String searchQuery, String businessPartnerCode, String startDate, String endDate,
    String headerAuthenticatorFbb) {
     Response responseData = ResponseHelper.status(HttpStatus.INTERNAL_SERVER_ERROR,null);
     responseData.setErrors(Map.of(ErrorMessages.FALLBACK_ERR_MESSAGE,
       ErrorCategory.COMMUNICATION_FAILURE.getMessage()));
     return responseData;
  }

  @Override
  public Response<List<CountConsignmentFormsByItemSkuResponse>> countInProgressConsignmentFormsByItemSkus(
    CountConsignmentFormsByItemSkusRequest webRequest,String headerAuthenticatorFbb) {
    Response responseData = ResponseHelper.status(HttpStatus.INTERNAL_SERVER_ERROR,null);
    responseData.setErrors(Map.of(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage()));
    return responseData;
  }
}