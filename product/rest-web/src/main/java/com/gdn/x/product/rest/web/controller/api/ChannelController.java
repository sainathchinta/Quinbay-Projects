package com.gdn.x.product.rest.web.controller.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.service.api.ChannelService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductApiPath.CHANNEL)
@Tag(name = "Channel", description = "Channel Service API")
public class ChannelController {

  @Autowired
  private ChannelService channelService;

  @Deprecated
  @RequestMapping(value = {ProductApiPath.GET}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SimpleListStringResponse> getListOfChannel(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username)
      throws Exception {
    SimpleListStringResponse response =
        new SimpleListStringResponse(this.channelService.getListOfChannel());
    return new GdnRestSingleResponse<SimpleListStringResponse>(response, requestId);
  }

}
