package com.gdn.partners.pbp.outbound.sap.feign;

import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface SapFeign {

  @RequestLine("GET /api/get-cogs?materialCode={materialCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<CogsValueResponse> getCogsValue(@Param("materialCode") String materialCode);

}