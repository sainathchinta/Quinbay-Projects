package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.response.ComboDetailResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PromoBundlingService;
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
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


@RestController
@RequestMapping(value = ProductApiPath.PROMO_BUNDLING)
@Tag(name = "Promo Bundling Controller", description = "Promo Bundling")
public class PromoBundlingController {

  private static final Logger LOG = LoggerFactory.getLogger(PromoBundlingController.class);

  @Autowired
  private PromoBundlingService promoBundlingService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ModelConverter modelConverter;

  @RequestMapping(value = ProductApiPath.GET_ACTIVE_COMBOS, method = RequestMethod.POST,
      produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get all active promo bundling combo", description = "get all promo bundling " +
      "combo from database")
  public GdnRestListResponse<ComboResponse> getActiveCombos(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ActiveComboRequest request,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "10") int size,
      @RequestParam(required = false, defaultValue = "createdDate") String sortBy,
      @RequestParam(required = false, defaultValue = "desc") String sortType) {
    try {
      ActiveComboRequestVO activeComboRequestVO = modelConverter
          .convertActiveComboRequestToActiveComboRequestVO(request);
      ComboResponseVO results = this.promoBundlingService.getActiveCombos(storeId, channelId,
          clientId, requestId, username, activeComboRequestVO, page, size, sortBy, sortType);

      List<ComboResponse> comboResponses =
          this.modelConverter.convertListToResponse(results.getComboList(), ComboResponse.class);
      return new GdnRestListResponse<ComboResponse>(null, null, true, comboResponses,
          new PageMetaData(size, page, results.getTotalRecords()), requestId);
    } catch (ApplicationRuntimeException e) {
      PromoBundlingController.LOG.error("#getActiveCombos with request {} ", request, e.getMessage());
      return new GdnRestListResponse<ComboResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_ACTIVE_COMBOS.getCode(), false, null, null, requestId);
    } catch (Exception e) {
      PromoBundlingController.LOG.error("#getActiveCombos with request {} ", request, e.getMessage(), e);
      return new GdnRestListResponse<ComboResponse>(e.getMessage(),
          ProductErrorCodesEnum.GET_ACTIVE_COMBOS.getCode(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_WHOLESALE_DETAIL, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get wholesale detail", description = "get wholesale detail from database")
  public GdnRestSingleResponse<WholesaleResponse> getWholesaleDetail(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(defaultValue = "false") boolean pristine,
      @RequestParam(defaultValue = "false") boolean includeNewBundlings) {
    WholesaleResponse wholesaleResponse = new WholesaleResponse();
    try {
      WholesaleVO result = this.promoBundlingService.getWholesaleByItemSku(storeId, channelId, clientId,
          requestId, username, itemSku, pristine, includeNewBundlings);
      wholesaleResponse = this.modelConverter.convertWholesaleVOToWholesaleResponse(result);
      return new GdnRestSingleResponse<>(null, null, true, wholesaleResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      PromoBundlingController.LOG.error("#getWholesaleDetail with itemSku {} ", itemSku, e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_WHOLESALE_DETAIL.getCode(), false, wholesaleResponse, requestId);
    } catch (Exception e) {
      PromoBundlingController.LOG.error("#getWholesaleDetail with itemSku {} ", itemSku, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_WHOLESALE_DETAIL.getCode(), false, wholesaleResponse, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_COMBO_DETAIL, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get promo bundling combo based on item sku",
      description = "get all promo bundling combo from database")
  public GdnRestSingleResponse<ComboDetailResponse> getComboDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String itemSku,
      @RequestParam(defaultValue = "false") boolean pristine,
      @RequestParam(defaultValue = "false") boolean includeNewBundlings) {
    try {
      ComboDetailVo comboDetailVo = promoBundlingService.getComboDetailByItemSku(storeId, channelId,
          clientId, requestId, username, itemSku, pristine, includeNewBundlings);
      ComboDetailResponse comboDetailResponse =
          modelConverter.convertComboDetailVoToComboDetailResponse(comboDetailVo);
      return new GdnRestSingleResponse<>(comboDetailResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      PromoBundlingController.LOG.error("#getComboDetail with itemSku {} ", itemSku,
          e.getMessage());
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_COMBO_DETAIL.getCode(), false, null, requestId);
    } catch (Exception e) {
      PromoBundlingController.LOG.error("#getComboDetail with itemSku {} ", itemSku,
          e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_COMBO_DETAIL.getCode(), false, null, requestId);
    }
  }
}