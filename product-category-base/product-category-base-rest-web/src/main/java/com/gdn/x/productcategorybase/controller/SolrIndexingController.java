package com.gdn.x.productcategorybase.controller;

import java.util.Calendar;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.service.solr.SolrIndexingService;
import com.gdn.x.productcategorybase.solr.service.ProductService;
import com.gdn.x.productcategorybase.util.ICalendarFactory;
import org.springframework.web.bind.annotation.RestController;

/**
 * Created by Kesha on 03/05/16.
 */
@Slf4j
@RestController
@RequestMapping(value = SolrIndexingController.BASE_PATH)
@Tag(name = "SolrIndexController", description = "Solr Reindexing API")
public class SolrIndexingController {
  public static final String BASE_PATH = "/api-solr/";
  private static final String REINDEXING = "product/reindex";
  private static final String XSCHEDULER = "XSCHEDULER";
  protected static final String REINDEX_BRAND_COLLECTION = "brand/reindex";
  protected static final String REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE = "brand/reindexByBrandRequestCode";
  protected static final String FULL_REINDEX = "fullReindex";
  protected static final String PARTIAL_REINDEX = "partialReindex";
  protected static final String REINDEX_PCB_COLLECTION = "pcb-collection/reindex";
  protected static final String REINDEX_PCB_COLLECTION_BY_PRODUCT_CODE = "pcb-collection/reindexByProductCode";

  protected static final String CATEGORY_BASED_REINDEX = "pcb-collection/reindex/category/{categoryCode}";

  @Autowired
  private ICalendarFactory calendarFactory;

  @Autowired
  private ProductService productService;

  @Autowired
  private SolrIndexingService solrIndexingService;

  private static boolean IS_FULLINDEX_DONE_ONCE = false;
  private boolean in_process = false;

  @RequestMapping(value = SolrIndexingController.REINDEXING, method = RequestMethod.GET)
  public GdnBaseRestResponse reIndex(@RequestParam String username,
      @RequestParam(defaultValue = "false") Boolean runFullIndex,
      @RequestParam(defaultValue = "false") Boolean skipFullIndex) {
    boolean status = false;
    if (XSCHEDULER.equals(username)) {
      Calendar calendar = calendarFactory.getInstance();
      if (!in_process) {
        in_process = true;
        if (!IS_FULLINDEX_DONE_ONCE || runFullIndex) {
            status=productService.fullIndex();
          IS_FULLINDEX_DONE_ONCE = true;
        } else if ((calendar.get(Calendar.HOUR_OF_DAY) == 0 && calendar.get(Calendar.MINUTE) < 30)
            && !skipFullIndex) {
          //run full indexing job
          status = productService.fullIndex();

        } else {
          //run delta indexing job
          status= productService.deltaIndex();
        }
        in_process = false;
      }
    }
    return new GdnBaseRestResponse(status);
  }

  @RequestMapping(value = SolrIndexingController.REINDEX_BRAND_COLLECTION, method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reIndexBrandCollection(@RequestParam String username,
      @RequestParam String reIndexType, @RequestParam("storeId") String storeId) throws Exception {
    if (FULL_REINDEX.equals(reIndexType)) {
      this.solrIndexingService.fullReindexBrandCollection(storeId);
      return new GdnBaseRestResponse(null, null, true, null);
    } else if (PARTIAL_REINDEX.equals(reIndexType)) {
      solrIndexingService.partialReindexBrandCollection(storeId);
      return new GdnBaseRestResponse(null, null, true, null);
    } else {
      return new GdnBaseRestResponse(null, ErrorMessage.INDEX_TYPE_DOES_NOT_MATCH.getMessage(), false, null);
    }
  }

  @RequestMapping(value = SolrIndexingController.REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE,
                  method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reIndexBrandCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String brandRequestCode)
      throws Exception {
    log.info("Reindex brand by brandRequestCode : {} ", brandRequestCode);
    this.solrIndexingService.reindexBrandCollectionByBrandRequestCode(storeId, brandRequestCode);
    return new GdnBaseRestResponse(null, null, true, null);
  }

  @RequestMapping(value = SolrIndexingController.REINDEX_PCB_COLLECTION, method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reIndexPcbCollection(@RequestParam String username,
      @RequestParam String reIndexType, @RequestParam("storeId") String storeId) throws Exception {
    if (FULL_REINDEX.equals(reIndexType)) {
      solrIndexingService.fullReindexPCBCollection(storeId);
      return new GdnBaseRestResponse(null, null, true, null);
    } else if (PARTIAL_REINDEX.equals(reIndexType)) {
      solrIndexingService.deltaReindexPCBCollection(storeId);
      return new GdnBaseRestResponse(null, null, true, null);
    } else {
      return new GdnBaseRestResponse(null, ErrorMessage.INDEX_TYPE_DOES_NOT_MATCH.getMessage(), false, null);
    }
  }

  @RequestMapping(value = SolrIndexingController.REINDEX_PCB_COLLECTION_BY_PRODUCT_CODE, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reIndexPcbCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode)
      throws Exception {
    log.info("Reindex pcb collection by productCode : {} ", productCode);
    solrIndexingService.deltaReindexPCBCollectionByProductCode(storeId, productCode);
    return new GdnBaseRestResponse(null, null, true, null);
  }

  @RequestMapping(value = SolrIndexingController.CATEGORY_BASED_REINDEX, method = RequestMethod.GET,
    produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse reIndexPcbCollectionByCategoryCode(@RequestParam String storeId,
    @PathVariable String categoryCode) throws Exception {
    log.info("re-index collection for category {}", categoryCode);

    solrIndexingService.categoryBasedReindexPCBCollection(storeId, categoryCode);
    return new GdnBaseRestResponse(null, null, true, null);
  }

}
