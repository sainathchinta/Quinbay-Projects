package com.gdn.x.product.rest.web.controller.api;

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

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.rest.web.model.ItemBuyableRequest;
import com.gdn.x.product.rest.web.model.ItemBuyableScheduleRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableScheduleRequest;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.service.api.ItemViewConfigService;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(value = ProductApiPath.ITEM_VIEW_CONFIG)
@Tag(name = "Item View Config", description = "Item View Config Service API")
public class ItemViewConfigController {

  private static final Logger LOG = LoggerFactory.getLogger(ItemController.class);

  @Autowired
  private ItemViewConfigService itemViewConfigService;

  @Autowired
  private GdnMapper gdnMapper;

  @RequestMapping(value = {ProductApiPath.UPDATE_BUYABLE_DEFAULT}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update buyable default",
      description = "update default buyable of item with specified itemSku")
  public GdnBaseRestResponse updateBuyableDefault(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestBody ItemBuyableRequest itemBuyableRequest) {

    ItemViewConfigController.LOG.info("Update buyable default with storeId = {}, and request = {}",
        new Object[] {storeId, itemBuyableRequest});
    try {
      this.itemViewConfigService.updateDefaultBuyable(storeId, itemBuyableRequest.getChannel(),
          itemSku, itemBuyableRequest.isBuyable());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ItemViewConfigController.LOG.error(
          "#updateBuyableDefault error with request = {}, error = {}", itemBuyableRequest,
          e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    } catch (Exception e) {
      ItemViewConfigController.LOG.error(
          "#updateBuyableDefault error with request = {}, error = {}", itemBuyableRequest,
          e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.UPDATE_BUYABLE_SCHEDULE}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update buyable schedule",
      description = "update buyable schedule to item with specified itemSku")
  public GdnBaseRestResponse updateBuyableSchedule(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestBody ItemBuyableScheduleRequest itemBuyableSchedule) {

    ItemViewConfigController.LOG.info(
        "Update buyable schedule with storeId = {}, and request = {}", new Object[] {storeId,
            itemBuyableSchedule});

    try {
      this.itemViewConfigService.updateBuyableSchedule(storeId, itemBuyableSchedule.getChannel(),
          itemSku, this.gdnMapper.deepCopy(itemBuyableSchedule, ItemBuyableSchedule.class));

      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ItemViewConfigController.LOG.error(
          "#updateBuyableSchedule error with request = {}, error = {}", itemBuyableSchedule,
          e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    } catch (Exception e) {
      ItemViewConfigController.LOG.error(
          "#updateBuyableSchedule error with request = {}, error = {}", itemBuyableSchedule,
          e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.UPDATE_DISCOVERABLE_DEFAULT},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update discoverable default",
      description = "update default discoverable of item with specified itemSku")
  public GdnBaseRestResponse updateDiscoverableDefault(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestBody ItemDiscoverableRequest itemDiscoverableRequest) {
    ItemViewConfigController.LOG.info(
        "Update discoverable default with storeId = {}, and request = {}", new Object[] {storeId,
            itemDiscoverableRequest});
    try {
      this.itemViewConfigService.updateDefaultDiscoverable(storeId,
          itemDiscoverableRequest.getChannel(), itemSku, itemDiscoverableRequest.isDiscoverable());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ItemViewConfigController.LOG.error(
          "#updateDiscoverableDefault error with request = {}, error = {}",
          itemDiscoverableRequest, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    } catch (Exception e) {
      ItemViewConfigController.LOG.error(
          "#updateDiscoverableDefault error with request = {}, error = {}",
          itemDiscoverableRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {ProductApiPath.UPDATE_DISCOVERABLE_SCHEDULE},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update discoverable schedule",
      description = "update discoverable schedule to item with specified itemSku")
  public GdnBaseRestResponse updateDiscoverableSchedule(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku,
      @RequestBody ItemDiscoverableScheduleRequest itemDiscoverableSchedule) {
    ItemViewConfigController.LOG.info(
        "Update discoverable schedule with storeId = {}, and request = {}", new Object[] {storeId,
            itemDiscoverableSchedule});
    try {
      this.itemViewConfigService.updateDiscoverableSchedule(storeId,
          itemDiscoverableSchedule.getChannel(), itemSku,
          this.gdnMapper.deepCopy(itemDiscoverableSchedule, ItemDiscoverableSchedule.class));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      ItemViewConfigController.LOG.error(
          "#updateDiscoverableSchedule error with request = {}, error = {}",
          itemDiscoverableSchedule, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    } catch (Exception e) {
      ItemViewConfigController.LOG.error(
          "#updateDiscoverableSchedule error with request = {}, error = {}",
          itemDiscoverableSchedule, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false, requestId);
    }
  }

}
