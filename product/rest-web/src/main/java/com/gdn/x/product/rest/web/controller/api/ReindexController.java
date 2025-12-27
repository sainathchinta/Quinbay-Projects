package com.gdn.x.product.rest.web.controller.api;

import java.util.List;
import java.util.concurrent.ExecutorService;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.service.api.ReindexService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = ProductApiPath.REINDEX)
@Tag(name = "Reindex Controller", description = "Reindex Solr Service API")
public class ReindexController {

  @Autowired
  private ReindexService reindexService;

  @Autowired
  private ExecutorService executorService;

  @RequestMapping(value = {ProductApiPath.REINDEX_AND_CLEARCACHE_BY_PRODUCT_SKUS},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse reindexSolrAndClearCacheByProductSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest listOfProductSku) throws Exception {
    this.reindexService.reindexSolrAndClearCacheByProductSkus(requestId, username, storeId,
        listOfProductSku.getValue());
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ProductApiPath.REINDEX_FULL}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse reindexSolrFull(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) throws Exception {
    this.reindexService.reindexFull(requestId, username, storeId);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.DELTA_REINDEX, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deltaReindex(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false) String indexFrom,
      @RequestParam(required = false) String indexTill)
      throws Exception {
    log.info("API to deltaReindex solr");
    this.reindexService.deltaReindex(storeId, requestId, username, indexFrom, indexTill);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.DELTA_REINDEX_ITEM_SKUS, method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deltaReindexItemSkus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username)
      throws Exception {
    log.info("API to deltaReindexItemSkus solr");
    try {
      this.reindexService.deltaReindexItemSkus(storeId);
    } catch (Exception e) {
      log.error("Exception caught while deltaReindexItemSkus", e);
      return new GdnBaseRestResponse(false);
    }
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ProductApiPath.REINDEX_FULL_SIMPLE}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse reindexSolrFullSimple(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) throws Exception {
    this.reindexService.reindexFullSimple(requestId, username, storeId);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ProductApiPath.REINDEX_SKU_SIMPLE}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse reindexSolrSkuSimple(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest listOfProductSku) throws Exception {
    this.reindexService.reindexSolrAndClearCacheByProductSkusSimple(requestId, username, storeId,
        listOfProductSku.getValue());
    return new GdnBaseRestResponse(true);
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD, method = RequestMethod.GET, produces
      = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse moveExistingSolrCollectionToCloud(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String sourceCollection,
      @RequestParam String destinationCollection) {
    try {
      reindexService.updateAll(sourceCollection, destinationCollection);
      return new GdnBaseRestResponse(Boolean.TRUE);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), null, Boolean.FALSE, null);
    }
  }


  @RequestMapping(value = ProductApiPath.REINDEX_PRODUCTS_TO_L3_COLLECTION, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reindexProductsToL3Collection(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String sourceCollection, @RequestParam String destinationCollection,
      @RequestParam(defaultValue = "ASC") String sortOrder,
      @RequestBody(required = false) List<String> categoryCodes) {
    try {
      reindexService
          .copyProductsToL3Collection(sourceCollection, destinationCollection, categoryCodes, sortOrder, storeId);
      return new GdnBaseRestResponse(Boolean.TRUE);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), null, Boolean.FALSE, null);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.REINDEX_DEFERRED_ITEMS, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reindexDeferredItemsFirstPage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) {
    log.info("API to reindex deferred items solr");
    try {
      reindexService.reindexDeferredItemsFirstPage(storeId);
    } catch (Exception e) {
      log.error("Exception caught while reindexing deferred items", e);
      return new GdnBaseRestResponse(false);
    }
    return new GdnBaseRestResponse(true);
  }


  @RequestMapping(value = ProductApiPath.REINDEX_DEFERRED_ITEMS_BY_TYPE, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reindexDeferredItemsFirstPageByType(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable(value = "reindexType") String reindexType,
      @RequestParam(required = false, defaultValue = "updatedDate") String orderBy,
      @RequestParam(required = false, defaultValue = "REINDEX_PENDING") String status) {
    log.info("API to reindex deferred items solr");
    try {
      reindexService.reindexDeferredItemsByReindexType(storeId, reindexType, orderBy, status);
    } catch (Exception e) {
      log.error("Exception caught while reindexing deferred items", e);
      return new GdnBaseRestResponse(false);
    }
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.DELTA_REINDEX_L3_COLLECTION, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deltaReindexToL3Collection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false) String indexFrom,
      @RequestParam(required = false) String indexTill)
      throws Exception {
    log.info("API to deltaReindex of new collection solr");
    this.reindexService.deltaReindexToL3Collection(storeId, indexFrom, indexTill);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.DELTA_REINDEX_ITEM_SKUS_BY_PRISTINE_DATA, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deltaReindexItemSkuByPristineData(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    log.info("API to deltaReindexItemSkus solr for pristineId");
    try {
      Pageable pageable = PageRequest.of(page, size);
      this.reindexService.deltaReindexItemSkuByPristine(storeId, pageable);
    } catch (Exception e) {
      log.error("Exception caught while deltaReindexItemSkus", e);
      return new GdnBaseRestResponse(false);
    }
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.POPULATE_NEW_FIELDS_L3_SOLR, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse populateNewFieldsL3Solr(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "5000") int maxReindexCount,
      @RequestParam(required = false) String reindexState) throws Exception {
    log.info(
        "API to populate new fields in L3 Solr collection. Max count : {}, Index for status : {}",
        maxReindexCount, reindexState);
    executorService.execute(() -> {
      try {
        this.reindexService
            .populateL3CollectionWithNewFields(storeId, maxReindexCount, reindexState);
      } catch (InterruptedException e) {
        log.error("Error on thread waiting between tasks - ", e);
      }
    });
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
