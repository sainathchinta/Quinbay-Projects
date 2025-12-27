package com.gdn.mta.product.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import org.apache.solr.common.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.enums.IndexTypes;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.service.solr.SolrIndexingService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.web.model.SolrReindexControllerPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

/**
 * This controller path is bypassing MandatoryRequestHolderFilter, see web.xml
 * 
 * @author andrew.winata
 */
@RestController
@RequestMapping(value = SolrReindexControllerPath.BASE_PATH)
@Tag(name = "SolrReindexController", description = "Solr Reindex Service API")
public class SolrReindexController {

  @Autowired
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private SolrIndexingService solrIndexingService;

  @Autowired
  private ProductService productService;

  @RequestMapping(value = SolrReindexControllerPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse moveExistingSolrCollectionToCloud(@RequestParam String sourceCollection,
      @RequestParam String destinationCollection) {
    try {
      solrIndexingService.updateAll(sourceCollection, destinationCollection);
      return new GdnBaseRestResponse(Boolean.TRUE);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), null, Boolean.FALSE, null);
    }
  }

  @RequestMapping(value = SolrReindexControllerPath.PRD_COLLECTION_DELETE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Delete from solr by product code",
      description = "Delete from solr by product code")
  @ResponseBody
  public GdnBaseRestResponse deleteFromSolrProductCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    this.solrActiveProductCollectionService.deleteFromSolrProductCollection(storeId, productCode);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.PRD_COLLECTION_SYNC_INACTIVE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Sync inactive product", description = "Sync inactive product")
  @ResponseBody
  public GdnBaseRestResponse syncInactiveProductCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    this.solrActiveProductCollectionService.syncInactiveProductCollection(storeId);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCTS, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Reindex screening products", description = "SReindex screening products")
  @ResponseBody
  public GdnBaseRestResponse reindexScreeningProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String indexType,
      @RequestParam(required = false) boolean isScreeningReindex) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    if(IndexTypes.FULL_REINDEX.getIndexType().equals(indexType)) {
      solrReviewProductCollectionService.fullReindexCollection(storeId, isScreeningReindex);
    } else if(IndexTypes.DELTA_REINDEX.getIndexType().equals(indexType)) {
      solrReviewProductCollectionService.deltaReindexCollection(storeId);
    } else {
      return new GdnBaseRestResponse("Invalid indexType",
          ErrorCategory.VALIDATION.getCode(), false, requestId);
    }
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.REINDEX_IN_REVIEW_PRODUCT_BY_PRODUCT_CODE, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Reindex screening product by product code", description = "SReindex screening product by product code")
  @ResponseBody
  public GdnBaseRestResponse reindexScreeningProductByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    GdnPreconditions
        .checkArgument(!StringUtils.isEmpty(productCode), ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    solrReviewProductCollectionService.deltaReindexProductCode(storeId, productCode);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Reindex active product collection by product code", description = "Reindex active product collection by product code")
  @ResponseBody
  public GdnBaseRestResponse reindexActiveProductByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    GdnPreconditions
        .checkArgument(!StringUtils.isEmpty(productCode), ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    productService.reindexActiveProductCollectionByStoreIdAndProductCode(storeId, productCode);
    return new GdnBaseRestResponse(true);
  }


  @RequestMapping(value = SolrReindexControllerPath.REINDEX_VARIANT_HISTORY, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Reindex variant history", description = "Reindex variant history")
  @ResponseBody
  public GdnBaseRestResponse reindexVariantHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody SimpleListStringRequest request) {
    solrIndexingService.reindexProductHistoryByProductSkus(storeId, request.getValue());
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.DELTA_REINDEX_VARIANT_HISTORY, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delta Reindex variant history", description = "Delta Reindex variant history")
  @ResponseBody
  public GdnBaseRestResponse deltaReindexVariantHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String startTime, @RequestParam(required = false) String endTime) throws Exception {
    solrIndexingService.deltaReindexHistoryCollection(storeId, startTime, endTime);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SolrReindexControllerPath.PRD_COLLECTION_DELTA_REINDEX, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delta Reindex for prd product collection", description = "Delta Reindex for prd product collection")
  public GdnBaseRestResponse deltaReindexPrdProductCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false) String indexFrom,
      @RequestParam(required = false) String indexTill) throws Exception {
    solrIndexingService.deltaReindexPrdProductCollection(storeId, indexFrom, indexTill);
    return new GdnBaseRestResponse(true);
  }


}
