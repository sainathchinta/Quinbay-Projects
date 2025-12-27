package com.gdn.partners.pcu.external.client.feign;

import com.blibli.oss.common.response.Response;
import com.gdn.fbb.core.constant.SortOrder;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.partners.pcu.external.client.factory.FBBFeignFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;


@FeignClient(name = "fbbFeign", url = "${service.fbb.endpoint}", fallbackFactory =
  FBBFeignFallbackFactory.class)
public interface FbbFeign {

  @RequestMapping(value = "/api/v3/consignment/forms/filter", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  Response<List<ConsignmentStatusResponse>> partnerConsignmentFormsByFilterForProduct(
    @RequestParam("sortBy") String sortBy, @RequestParam("sortOrder") SortOrder sortOrder,
    @RequestParam("page") Integer page, @RequestParam("itemPerPage") Integer itemPerPage,
    @RequestParam("searchQuery") String searchQuery,
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestParam("startDate") String startDate,
    @RequestParam("endDate") String endDate,
    @RequestHeader(name = "header-authenticator") String headerAuthenticatorFbb
    );


  @RequestMapping(value = "/api/v3/consignment/forms/in-progress/_count", method =
    RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes =
    MediaType.APPLICATION_JSON_VALUE)
  Response<List<CountConsignmentFormsByItemSkuResponse>> countInProgressConsignmentFormsByItemSkus(
    @RequestBody CountConsignmentFormsByItemSkusRequest webRequest,
    @RequestHeader(name = "header-authenticator") String headerAuthenticatorFbb);



}
