package com.gdn.x.product.rest.web.controller.api;

import java.util.List;
import java.util.Map;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.vo.Off2OnPriceVO;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponse;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponseV2;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.service.api.Off2OnService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductApiPath.OFF2ON)
@Tag(name = "Off2On Controller", description = "Offline to Online Service API")
public class Off2OnController {

  private static final Logger LOG = LoggerFactory.getLogger(Off2OnController.class);

  @Autowired
  private Off2OnService off2OnService;

  @Autowired
  private ModelConverter modelConverter;

  @RequestMapping(value = ProductApiPath.ACTIVATE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "activate off2OnChannelActive",
      description = "activate off2OnChannelActive on certain items")
  public GdnBaseRestResponse activateOff2OnChannel(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku) {
    try {
      boolean activateOff2OnChannel =
          this.off2OnService.activateOff2OnChannelByItemSku(storeId, itemSku);
      return new GdnBaseRestResponse(activateOff2OnChannel);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error("#activateOff2OnChannel of itemSku = {} failed with error = {}",
          itemSku, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error("#activateOff2OnChannel of itemSku = {} failed with error = {}",
          itemSku, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ACTIVATE_BULK, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "activate off2OnChannelActive bulk",
      description = "activate off2OnChannelActive on certain items")
  public GdnRestSingleResponse<SimpleListStringResponse> activateOff2OnChannelBulk(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest itemSkuRequest) {
    try {
      List<String> result =
          this.off2OnService.activateOff2OnChannelByItemSku(storeId, itemSkuRequest.getValue());
      return new GdnRestSingleResponse<SimpleListStringResponse>(new SimpleListStringResponse(
          result), requestId);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error(
          "#activateOff2OnChannelBulk of itemSku = {} failed with error = {}", itemSkuRequest,
          e.getMessage());
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "#activateOff2OnChannelBulk of itemSku = {} failed with error = {}", itemSkuRequest,
          e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    }
  }


  @RequestMapping(value = ProductApiPath.ACTIVATE_BY_MERCHANT_CODE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "activate off2OnChannelActive",
      description = "activate off2OnChannelActive on certain items")
  public GdnRestSingleResponse<SimpleListStringResponse> activateOff2OnChannelByMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String merchantCode) {
    try {
      List<String> failedList =
          this.off2OnService.activateOff2OnChannelByMerchantCode(storeId, merchantCode);
      return new GdnRestSingleResponse<SimpleListStringResponse>(new SimpleListStringResponse(
          failedList), requestId);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error(
          "#activateOff2OnChannelByMerchantCode of merchantCode = {} failed with error = {}",
          merchantCode, e.getMessage());
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "#activateOff2OnChannelByMerchantCode of merchantCode = {} failed with error = {}",
          merchantCode, e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DEACTIVATE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "deactivate off2OnChannelActive",
      description = "deactivate off2OnChannelActive on certain items")
  public GdnBaseRestResponse deactivateOff2OnChannel(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku) {
    try {
      boolean activateOff2OnChannel =
          this.off2OnService.deactivateOff2OnChannelByItemSku(storeId, itemSku);
      return new GdnBaseRestResponse(activateOff2OnChannel);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error("#deactivateOff2OnChannel of itemSku = {} failed with error = {}",
          itemSku, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error("#deactivateOff2OnChannel of itemSku = {} failed with error = {}",
          itemSku, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DEACTIVATE_BULK, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "deactivate off2OnChannelActive bulk",
      description = "deactivate off2OnChannelActive on certain items")
  public GdnRestSingleResponse<SimpleListStringResponse> deactivateOff2OnChannelBulk(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleListStringRequest itemSkuRequest) {
    try {
      List<String> result =
          this.off2OnService.deactivateOff2OnChannelByItemSku(storeId, itemSkuRequest.getValue());
      return new GdnRestSingleResponse<SimpleListStringResponse>(new SimpleListStringResponse(
          result), requestId);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error(
          "#deactivateOff2OnChannelBulk of itemSku = {} failed with error = {}", itemSkuRequest,
          e.getMessage());
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "#deactivateOff2OnChannelBulk of itemSku = {} failed with error = {}", itemSkuRequest,
          e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DEACTIVATE_BY_MERCHANT_CODE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "deactivate off2OnChannelActive by merchant code",
      description = "deactivate off2OnChannelActive of merchant")
  public GdnRestSingleResponse<SimpleListStringResponse> deactivateOff2OnChannelByMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String merchantCode) {
    try {
      List<String> failedList =
          this.off2OnService.deactivateOff2OnChannelByMerchantCode(storeId, merchantCode);
      return new GdnRestSingleResponse<SimpleListStringResponse>(new SimpleListStringResponse(
          failedList), requestId);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error(
          "#deactivateOff2OnChannelByMerchantCode of merchantCode = {} failed with error = {}",
          merchantCode, e.getMessage());
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "#deactivateOff2OnChannelByMerchantCode of merchantCode = {} failed with error = {}",
          merchantCode, e.getMessage(), e);
      return new GdnRestSingleResponse<SimpleListStringResponse>(e.getMessage(),
          ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.GET_OFF2ON_PRICE}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<Off2OnPriceResponse> getPriceAndOff2OnChannelActiveByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam Map<String, String> requestParams, @RequestParam String channel,
      @RequestBody SimpleListStringRequest itemSkuList) {
    try {
      List<Off2OnPriceVO> result =
          this.off2OnService.getProductPriceForOff2On(storeId, itemSkuList.getValue(), channel);
      List<Off2OnPriceResponse> response =
          this.modelConverter.convertListToResponse(result, Off2OnPriceResponse.class);
      return new GdnRestListResponse<Off2OnPriceResponse>(response, new PageMetaData(
          response.size(), 0, response.size()), requestId);
    } catch (ApplicationRuntimeException e) {
      Off2OnController.LOG.error(
          "#getPriceAndOff2OnChannelActiveByItemSku of itemSkuList = {} failed with error = {}",
          itemSkuList, e.getMessage());
      return new GdnRestListResponse<Off2OnPriceResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRICE_OFF2ON.getCode(), false, requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "#getPriceAndOff2OnChannelActiveByItemSku of itemSkuList = {} failed with error = {}",
          itemSkuList, e.getMessage(), e);
      return new GdnRestListResponse<Off2OnPriceResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRICE_OFF2ON.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_OFF2ON_PRICE_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<Off2OnPriceResponseV2> getPriceAndOff2OnChannelActiveByItemSkuAndPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String channel,
      @RequestParam(required = false, defaultValue = "false") boolean needActivePromoBundlings,
      @RequestBody List<ItemPickupPointRequest> itemPickupPointRequest) {
    try {
      List<Off2OnPriceResponseV2> result =
          this.off2OnService.getProductPriceForOff2OnV2(storeId, itemPickupPointRequest, channel, needActivePromoBundlings);
      return new GdnRestListResponse<Off2OnPriceResponseV2>(result, new PageMetaData(result.size(), 0, result.size()),
          requestId);
    } catch (Exception e) {
      Off2OnController.LOG.error(
          "error while getPriceAndOff2OnChannelActiveByItemSkuAndPickupPointCode of request = {} ",
          itemPickupPointRequest, e);
      return new GdnRestListResponse<Off2OnPriceResponseV2>(e.getMessage(),
          ProductErrorCodesEnum.GET_PRICE_OFF2ON.getCode(), false, requestId);
    }
  }

}
