package com.gdn.partners.pbp.controller.mv;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.IndexingRequest;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.service.mv.indexing.FullIndexingServiceBean;
import com.gdn.partners.pbp.service.mv.indexing.IndexingService;
import com.gdn.partners.pbp.service.mv.indexing.PartialByBusinessPartnerCodeServiceBean;
import com.gdn.partners.pbp.service.mv.indexing.PartialByItemSKUsServiceBean;
import com.gdn.partners.pbp.web.model.ProductAggregatorIndexingControllerPath;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductAggregatorIndexingControllerPath.BASE_PATH)
@Tag(name = "Product Aggregator Indexing",
    description = "Merchant Product Aggregator Indexing Service API.")
public class ProductAggregatorIndexingController {

  @Autowired
  @Qualifier(PartialByBusinessPartnerCodeServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
  private IndexingService partialByBusinessPartnerCodeService;

  @Autowired
  @Qualifier(PartialByItemSKUsServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
  private IndexingService partialByItemSkusService;

  @Autowired
  @Qualifier(FullIndexingServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
  private IndexingService fullIndexingService;

  @SuppressWarnings("unchecked")
  @RequestMapping(value = ProductAggregatorIndexingControllerPath.PARTIAL_INDEXING_ITEM_SKUS_PATH,
      method = RequestMethod.POST, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Partial indexing",
      description = "Partial indexing by item skus, body example {\"request\":{\"1234\":[\"SKU1\",\"SKU2\"]}}"
          + "")
  public GdnBaseRestResponse partialIndexingByItemSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody IndexingRequest requestBody,
      @RequestParam boolean isForce) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    Map<String, Object> requestMap = new HashMap<>();
    for (String businessPartnerCode : requestBody.getRequest().keySet()) {
      requestMap.put(businessPartnerCode,
          (List<String>) requestBody.getRequest().get(businessPartnerCode));
    }
    this.partialByItemSkusService.doIndexing(mandatoryRequestParam, requestMap, isForce);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductAggregatorIndexingControllerPath.PARTIAL_INDEXING_BUSINESS_PARTNER_CODES_PATH,
      method = RequestMethod.POST, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Partial indexing", description = "Partial indexing by business partner code")
  public GdnBaseRestResponse partialIndexingByBusinessPartnerCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam boolean isForce, @RequestBody IndexingRequest requestBody)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    this.partialByBusinessPartnerCodeService.doIndexing(mandatoryRequestParam, requestBody.getRequest(), isForce);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductAggregatorIndexingControllerPath.FULL_INDEXING_PATH, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Full indexing", description = "Full indexing")
  public GdnBaseRestResponse fullIndexing(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam boolean isForce) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    this.fullIndexingService.doIndexing(mandatoryRequestParam, null, isForce);
    return new GdnBaseRestResponse(requestId);
  }

}
