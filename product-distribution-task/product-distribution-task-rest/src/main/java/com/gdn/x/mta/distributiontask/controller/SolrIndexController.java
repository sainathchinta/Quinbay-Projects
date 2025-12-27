package com.gdn.x.mta.distributiontask.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.rest.model.SolrIndexControllerPath;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@Slf4j
@RequestMapping(value = SolrIndexControllerPath.BASE_PATH)
@Tag(name = "SolrIndexController", description = "Reindex pdt solr collections")
public class SolrIndexController {

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @RequestMapping(value = SolrIndexControllerPath.FULL_REINDEX_PDT_SOLR, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation
  public GdnBaseRestResponse fullReindexPDTProductSolr(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("Starting full reindex of solr for pdt_product, requestId : {}", requestId);
    solrVendorCollectionService.fullReindexPDTProductSolr(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = SolrIndexControllerPath.DELTA_REINDEX_PDT_SOLR, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation
  public GdnBaseRestResponse deltaReindexPDTProductSolr(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String productCode) {
    log.info("Starting full reindex of solr for pdt_product, requestId : {}", requestId);
    solrVendorCollectionService.deltaReindexPDTProductSolr(storeId, productCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
