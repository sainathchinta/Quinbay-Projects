package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.response.DefaultItemSkuResponse;
import com.gdn.x.product.rest.web.model.response.PristineItemDetailAndMapping;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataResponse;
import com.gdn.x.product.service.PristineService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(value = ProductApiPath.PRISTINE)
@Tag(name = "Pristine Controller",
    description = "Pristine Service API")
public class PristineController {
  private static final Logger LOGGER = LoggerFactory.getLogger(PristineController.class);

  @Autowired
  private PristineService pristineService;

  @Autowired
  private GdnMapper gdnMapper;

  @RequestMapping(value = ProductApiPath.PRISTINE_ITEM_MAP_BY_ID,
      method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get pristine item detail and also get similar pristine mapped to same "
      + "master pristine attributes")
  public GdnRestSingleResponse<PristineItemDetailAndMapping> getMapForPristineItemsById(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String pristineId,
      @RequestParam(required = false) String defaultSku) throws Exception {
    MandatoryRequestParam param = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    LOGGER.info("Get Pristine Item Mapping for pristineId {}", pristineId);
    PristineItemDetailAndMapping pristineResponse = new PristineItemDetailAndMapping();
    try {
      PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo =
          pristineService.getPristineItemsMappingByPristineId(param, pristineId, defaultSku);
      pristineResponse = gdnMapper.deepCopy(pristineItemDetailAndMappingVo, PristineItemDetailAndMapping.class);
      return new GdnRestSingleResponse<>(null, null, true, pristineResponse, requestId);
    } catch (Exception e) {
      LOGGER
          .error("failed to fetch pristine category attribute map : requestId :{} " + requestId, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.GET_PRISTINE_DATA.getCode(), false, pristineResponse, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID,
   method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get default item sku", description = "Get default item sku")
  public GdnRestSingleResponse<DefaultItemSkuResponse> getDefaultItemSkuByPristineId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String pristineId) {
    LOGGER.info("Get default item sku by pristineId = {}", pristineId);
    DefaultItemSkuResponse response = new DefaultItemSkuResponse();
    try {
      MandatoryRequestParam param = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
      param.setUsername(username);
      DefaultItemSkuVO defaultItemSkuVo = pristineService.getDefaultItemSkuByPristineId(param, pristineId);
      BeanUtils.copyProperties(defaultItemSkuVo, response);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY,
          true, response, requestId);
    }
    catch (ApplicationRuntimeException e){
      LOGGER.error("failed to fetch default item sku for pristineId = {}",
          pristineId, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID.getMessage(),
          ProductErrorCodesEnum.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID.getCode(), false, null, requestId);
    }
    catch (Exception e) {
      LOGGER.error("failed to fetch default item sku for pristineId = {}",
          pristineId, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {
      ProductApiPath.GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU},
      method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get pristine master Id", description = "Get pristine master Id")
  public GdnRestSingleResponse<PristineMasterDataResponse> getPristineMasterIdByPristineIdOrProductCodeOrSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String pristineIdOrProductCodeOrSku) {
    try {
      LOGGER.info("Get pristine master Id with storeId = {}, pristineIdOrProductCodeOrSku = {}",
          storeId, pristineIdOrProductCodeOrSku);
      PristineMasterDataResponse response = new PristineMasterDataResponse();
      response.setPristineMasterId(
          this.pristineService.getPristineMasterIdByPristineIdOrProductCodeOrSku(
              storeId, username, requestId, pristineIdOrProductCodeOrSku));
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error(" Error while getting pristine master id with storeId = {}, pristineIdOrProductCodeOrSku = {}," +
          " errorMessage: {}", storeId, pristineIdOrProductCodeOrSku, e.getErrorMessage());
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.GET_PRISTINE_MASTER_ID.getMessage(),
          ProductErrorCodesEnum.GET_PRISTINE_MASTER_ID.getCode(), false, null, requestId);
    } catch (Exception e) {
      LOGGER.error(" Error while getting pristine master id with storeId = {}, pristineIdOrProductCodeOrSku = {}," +
          " errorMessage: {}", storeId, pristineIdOrProductCodeOrSku, e.getMessage());
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, null, requestId);
    }
  }
}
