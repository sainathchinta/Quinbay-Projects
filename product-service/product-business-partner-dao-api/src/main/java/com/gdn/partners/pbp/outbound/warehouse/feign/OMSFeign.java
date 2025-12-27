package com.gdn.partners.pbp.outbound.warehouse.feign;

import com.blibli.oss.backend.common.model.response.Response;
import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

import java.util.List;

public interface OMSFeign {

  @RequestLine("POST/api/oms/stock-visibility/validate-uom-editable?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  Response<List<UomStockValidationResponse>> validateUomEditable(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      UomStockValidationRequest request);
}
