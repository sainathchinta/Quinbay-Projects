package com.gdn.x.product.rest.web.controller.api;


import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.util.LuceneOptimizeService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductApiPath.UTIL)
@Tag(name = "Util Controller", description = "Utility API for internal uses")
public class UtilController {

  @Autowired
  @Qualifier("solrOptimizeServiceImpl")
  private LuceneOptimizeService optimizeService;

  @Autowired
  private ItemService itemService;

  @RequestMapping(value = {ProductApiPath.OPTIMIZE_INDEX}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnBaseRestResponse optimize(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) throws Exception {
    this.optimizeService.optimize();
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ProductApiPath.SEND_ITEM_CHANGE_BY_UPDATED_BY}, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse sendItemChangeByUpdatedBy(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String updatedBy) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(updatedBy),
        "UpdatedBy should not be empty");
    this.itemService.sendItemChangeEventByUpdatedBy(storeId, updatedBy);
    return new GdnBaseRestResponse(true);
  }

}
