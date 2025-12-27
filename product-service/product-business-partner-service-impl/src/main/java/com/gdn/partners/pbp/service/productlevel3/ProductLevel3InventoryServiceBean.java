package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdateSyncStockByWebItemPickupPointRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.UpdateOrInsertStockVo;
import com.gda.mta.product.dto.InventoryUpsertModel;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.SyncStockUpdateOrInsertVo;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dao.WebInventoryInsertRequestV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3InventoryRepository;
import com.gdn.partners.pbp.util.ProductLevel3InventoryUtil;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.model.vo.request.InventoryBaseRequestVo;
import com.gdn.x.inventory.v2.rest.web.model.enums.ActionKeyEnum;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductLevel3InventoryServiceBean implements ProductLevel3InventoryService {

  @Autowired
  private ProductLevel3InventoryRepository productLevel3InventoryRepository;
  @Autowired
  private ProductLevel3Converter productLevel3Converter;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${inventory.L5.sync.stock.enabled}")
  private boolean inventoryL5SyncStockEnabled;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${insert.inventory.enabled.on.failed.update}")
  private boolean insertInventoryEnabledOnFailedUpdate;

  @Value("${insert.inventory.enabled.on.failed.sync.stock.update}")
  private boolean insertInventoryEnabledOnFailedSyncStockUpdate;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Override
  public ProductLevel3Inventory findInventoryByBusinessPartnerCodeAndGdnSku(
      String businessPartnerCode, String gdnSku) throws Exception {
    List<String> gdnSkus = new ArrayList<>();
    gdnSkus.add(gdnSku);
    List<ProductLevel3Inventory> inventories =
        this.findInventoryByBusinessPartnerCodeAndListOfGdnSku(businessPartnerCode, gdnSkus);
    if (CollectionUtils.isEmpty(inventories)) {
      return null;
    } else {
      if (inventories.size() > 1) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
            "Found inventory more than one record, with businessPartnerCode: "
                + businessPartnerCode + ", gdnSku: " + gdnSku);
      }
      return inventories.get(0);
    }
  }

  private Map<String, List<ItemSkuPickupPointCodeResponse>> getItemSkusToPickupPointMap(List<String> gdnSkus) {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> pickupPointCodeByItemSkus =
        xProductOutbound.getItemPickupPointCodeByItemSkus(new SimpleListStringRequest(gdnSkus));
    if (!pickupPointCodeByItemSkus.isSuccess() && CollectionUtils.isEmpty(pickupPointCodeByItemSkus.getContent())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No delivery true pickupPoint found");
    }
    return pickupPointCodeByItemSkus.getContent().stream()
        .collect(Collectors.groupingBy(ItemSkuPickupPointCodeResponse::getItemSku));
  }

  @Override
  public List<ProductLevel3Inventory> findInventoryByGdnSkuMap(Map<String, String> gdnSkuMap)
      throws Exception {
    if (MapUtils.isEmpty(gdnSkuMap)) {
      return new ArrayList<>();
    }
    List<InventoryDetailInfoRequestDTO> request = new ArrayList<>();
    Map<String, List<ItemSkuPickupPointCodeResponse>> itemSkusToPickupPointMap =
        getItemSkusToPickupPointMap(new ArrayList<>(gdnSkuMap.keySet()));
    gdnSkuMap.keySet().stream().forEach(sku -> {
      InventoryDetailInfoRequestDTO itemRequest = new InventoryDetailInfoRequestDTO();
      itemRequest.setWebItemSku(sku);
      itemRequest.setWebMerchantCode(gdnSkuMap.get(sku));
      itemRequest.setInventoryBaseRequest(new InventoryBaseRequest());
      if (itemSkusToPickupPointMap.containsKey(sku) && itemSkusToPickupPointMap.get(sku).size() == 1) {
        itemRequest.setPickupPointCode(itemSkusToPickupPointMap.get(sku).get(0).getPickupPointCode());
        request.add(itemRequest);
      }
    });
    if (CollectionUtils.isEmpty(request)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Inventory not found for given gdnSkus: " + gdnSkuMap.keySet());
    }
    List<InventoryDetailInfoResponseV2DTO> inventoryDetails =
        this.productLevel3InventoryRepository.findDetailByBusinessPartnerCodeAndGdnSku(
            ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    return inventoryDetails
        .stream()
        .map(
            inv -> this.productLevel3Converter
                .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(inv))
        .collect(Collectors.toList());
  }


  @Override
  public List<ProductLevel3Inventory> findInventoryByBusinessPartnerCodeAndListOfGdnSku(
      String businessPartnerCode, List<String> gdnSkus) throws Exception {
    if (CollectionUtils.isEmpty(gdnSkus)) {
      return new ArrayList<>();
    }
    Map<String, List<ItemSkuPickupPointCodeResponse>> itemSkusToPickupPointMap = getItemSkusToPickupPointMap(gdnSkus);
    List<InventoryDetailInfoRequestDTO> request = new ArrayList<>();
    gdnSkus.stream().forEach(sku -> {
      InventoryDetailInfoRequestDTO itemRequest = new InventoryDetailInfoRequestDTO();
      itemRequest.setWebItemSku(sku);
      itemRequest.setWebMerchantCode(businessPartnerCode);
      itemRequest.setInventoryBaseRequest(new InventoryBaseRequest());
      if (itemSkusToPickupPointMap.containsKey(sku) && itemSkusToPickupPointMap.get(sku).size() == 1) {
        itemRequest.setPickupPointCode(itemSkusToPickupPointMap.get(sku).get(0).getPickupPointCode());
        request.add(itemRequest);
      }
    });
    if (CollectionUtils.isEmpty(request)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Inventory not found for given gdnSkus: " + gdnSkus);
    }
    List<InventoryDetailInfoResponseV2DTO> inventoryDetails =
        this.productLevel3InventoryRepository.findDetailByBusinessPartnerCodeAndGdnSku(
            ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    return inventoryDetails
        .stream()
        .map(
            inv -> this.productLevel3Converter
                .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(inv))
        .collect(Collectors.toList());
  }

  @Override
  public List<ProductLevel3Inventory> findInventoryByInventoryFilter(String businessPartnerCode, Integer stock,
      ProductLevel3InventoryCriteria inventoryCriteria) throws Exception {
    List<InventoryDetailInfoResponseDTO> inventoryDetails = new ArrayList<>();
    if (stock != null) {
      inventoryDetails = this.productLevel3InventoryRepository.findDetailByBusinessPartnerCodeAndStock(
          ProductLevel3InventoryUtil.generateMandatoryRequestParam(), businessPartnerCode, stock);
    }
    return inventoryDetails.stream()
        .map(inv -> this.productLevel3Converter.convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(inv))
        .collect(Collectors.toList());
  }

  @Override
  public void insertInventory(List<ProductLevel3Inventory> inventories) throws Exception {
    if (CollectionUtils.isEmpty(inventories)) {
      return;
    }
    List<WebInventoryInsertRequestDTO> request =
        inventories
            .stream()
            .filter(inv -> inv != null)
            .map(
                inv -> {
                  WebInventoryInsertRequestV2DTO item = new WebInventoryInsertRequestV2DTO();
                  item.setWebItemSku(inv.getWebItemSku());
                  item.setWebMerchantCode(inv.getWebMerchantCode());
                  item.setWarehouseItemSku(inv.getWarehouseItemSku());
                  item.setWarehouseMerchantCode(inv.getWarehouseMerchantCode());
                  item.setOriginalStock(inv.getWebAvailable());
                  item.setAvailableStock(inv.getWebAvailable());
                  item.setMinimumStockAlert(inv.getWebMinAlert());
                  item.setSyncStock(inv.isWebSyncStock());
                  item.setPickupPointCode(inv.getWebPickupPointCode());
                  item.setInventoryBaseRequest(ProductLevel3InventoryUtil
                      .generateInventoryBaseRequest(Constants.UPDATE_PRE_ORDER_QUOTA));
                  item.setWebProductSku(inv.getProductSku());
                  item.setFbbPP(inv.isFbbPP() && mppForWhEnabled);
                  item.setDistributionPickupPoint(inv.isDistributionPickupPoint());
                  item.setPreOrderEndDate(inv.getPreOrderDate());
                  item.setInitialPoQuota(inv.getInitialPreOrderQuota());
                  return item;
                }).collect(Collectors.toList());
    this.productLevel3InventoryRepository.insertInventory(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
  }
  @Override
  public void updateMinimumStockAlert(String businessPartnerCode, String gdnSku,
      Integer minimumStockAlert) throws Exception {
    WebInventoryUpdateMinimumStockAlertRequestDTO request =
        new WebInventoryUpdateMinimumStockAlertRequestDTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setMinimumStockAlert(minimumStockAlert);
    request.setInventoryBaseRequest(new InventoryBaseRequest());
    this.productLevel3InventoryRepository.updateMinimumStockAlert(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
  }

  @Override
  public void updatePickupPoint(String businessPartnerCode, String gdnSku, String pickupPointCode)
      throws Exception {
    WebInventoryUpdatePickupPointCodeRequestDTO request =
        new WebInventoryUpdatePickupPointCodeRequestDTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setPickupPointCode(pickupPointCode);
    request.setInventoryBaseRequest(new InventoryBaseRequest());
    this.productLevel3InventoryRepository.updatePickupPoint(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
  }

  @Override
  public void updateStock(String businessPartnerCode, String gdnSku, Integer deltaStock)
      throws Exception {
    if ((deltaStock == null) || (deltaStock == 0)) {
      return;
    }
    WebInventoryUpdateStockRequestV2DTO request = new WebInventoryUpdateStockRequestV2DTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setStock(Math.abs(deltaStock));
    request.setInventoryBaseRequest(ProductLevel3InventoryUtil
        .generateInventoryBaseRequest(ActionKeyEnum.UPDATE_WEB_STOCK));
    setPickupPointCodeInRequest(gdnSku, request);
    if (deltaStock < 0) {
      this.productLevel3InventoryRepository.updateStockDecrease(
          ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    } else {
      this.productLevel3InventoryRepository.updateStockIncrease(
          ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    }
  }

  @Override
  public ApiErrorCode updateStockV2(String businessPartnerCode, String gdnSku, Integer deltaStock) throws Exception {
    ApiErrorCode apiErrorCode = null;
    if ((deltaStock == null) || (deltaStock == 0)) {
      return apiErrorCode;
    }
    WebInventoryUpdateStockRequestV2DTO request = new WebInventoryUpdateStockRequestV2DTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setStock(Math.abs(deltaStock));
    request.setInventoryBaseRequest(
        ProductLevel3InventoryUtil.generateInventoryBaseRequest(ActionKeyEnum.UPDATE_WEB_STOCK));
    setPickupPointCodeInRequest(gdnSku, request);
    if (deltaStock < 0) {
      this.productLevel3InventoryRepository
          .updateStockDecrease(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    } else {
      apiErrorCode = this.productLevel3InventoryRepository
          .updateStockIncreaseV2(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    }
    return apiErrorCode;
  }

  private void setPickupPointCodeInRequest(String gdnSku, WebInventoryUpdateStockRequestDTO request) {
    Map<String, List<ItemSkuPickupPointCodeResponse>> itemSkusToPickupPointMap =
        getItemSkusToPickupPointMap(Arrays.asList(gdnSku));
    if (itemSkusToPickupPointMap.containsKey(gdnSku) && itemSkusToPickupPointMap.get(gdnSku).size() == 1) {
      request.setPickupPointCode(itemSkusToPickupPointMap.get(gdnSku).get(0).getPickupPointCode());
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Inventory not found for given gdnSku: " + gdnSku);
    }
  }

  @Override
  public void updateSyncStockByBusinessPartnerCode(String businessPartnerCode, boolean syncStock)
      throws Exception {
    UpdateSyncStockByWebMerchantCodeRequestDTO request =
        new UpdateSyncStockByWebMerchantCodeRequestDTO();
    request.setWebMerchantCode(businessPartnerCode);
    request.setSyncStock(syncStock);
    request.setInventoryBaseRequest(new InventoryBaseRequest());
    this.productLevel3InventoryRepository.updateSyncStockByBusinessPartnerCode(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
  }

  @Override
  public void updateSyncStockByBusinessPartnerCodeAndGdnSku(String businessPartnerCode, String gdnSku,
      boolean syncStock, List<ItemSkuPickupPointSyncStockDto> pickupPointSyncStockDtoList) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = ProductLevel3InventoryUtil.generateMandatoryRequestParam();
    if (inventoryL5SyncStockEnabled) {
      List<PickupPointResponse> pickupPointResponseList = pickupPointOutbound
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, new ArrayList<>(
              pickupPointSyncStockDtoList.stream().map(ItemSkuPickupPointSyncStockDto::getPickupPointCode)
                  .collect(Collectors.toSet())));
      Map<String, Boolean> pickupPointToFbbMap = pickupPointResponseList.stream()
          .collect(Collectors.toMap(PickupPointResponse::getCode, PickupPointResponse::isFbbActivated));
      List<UpdateSyncStockByWebItemPickupPointRequest> listRequest = new ArrayList<>();
      for (ItemSkuPickupPointSyncStockDto itemSkuPickupPointSyncStockDto : pickupPointSyncStockDtoList) {
        if (pickupPointToFbbMap.getOrDefault(itemSkuPickupPointSyncStockDto.getPickupPointCode(), false)) {
          UpdateSyncStockByWebItemPickupPointRequest request = new UpdateSyncStockByWebItemPickupPointRequest();
          request.setWebItemSku(itemSkuPickupPointSyncStockDto.getItemSku());
          request.setWebMerchantCode(businessPartnerCode);
          request.setSyncStock(itemSkuPickupPointSyncStockDto.isSyncStock());
          request.setStock(Optional.ofNullable(itemSkuPickupPointSyncStockDto.getStock()).orElse(Constants.ZERO));
          request.setPickupPointCode(itemSkuPickupPointSyncStockDto.getPickupPointCode());
          request.setInventoryBaseRequest(new InventoryBaseRequestVo());
          listRequest.add(request);
        }
      }
      if (CollectionUtils.isNotEmpty(listRequest)) {
        this.productLevel3InventoryRepository
            .updateSyncStockAtL5(mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
                mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
                mandatoryRequestParam.getUsername(), listRequest);
      }
    } else {
      UpdateSyncStockByWebItemSkuRequestDTO request = new UpdateSyncStockByWebItemSkuRequestDTO();
      request.setWebItemSku(gdnSku);
      request.setWebMerchantCode(businessPartnerCode);
      request.setSyncStock(syncStock);
      request.setInventoryBaseRequest(new InventoryBaseRequest());
      this.productLevel3InventoryRepository
          .updateSyncStockByBusinessPartnerCodeAndGdnSku(mandatoryRequestParam, request);
    }
  }

  @Override
  public List<ProductLevel3Inventory> findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
      ProductLevel3InventoryUtil.generateMandatoryRequestParam();
    List<InventoryDetailInfoResponseV2DTO> inventoryDetails =
      this.productLevel3InventoryRepository.findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
        mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), inventoryDetailInfoRequestDTOList);
    return inventoryDetails.stream().map(
      inv -> this.productLevel3Converter.convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
        inv)).collect(Collectors.toList());
  }

  @Override
  public ApiErrorCode updateStockForItemPickupPointWithErrorCode(String businessPartnerCode, String gdnSku,
      String pickupPointCode, Integer deltaStock) throws Exception {
    ApiErrorCode apiErrorCode = null;
    if (Objects.isNull(deltaStock) || Integer.valueOf(0).equals(deltaStock)) {
      return apiErrorCode;
    }
    WebInventoryUpdateStockRequestV2DTO request = new WebInventoryUpdateStockRequestV2DTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setStock(Math.abs(deltaStock));
    request.setInventoryBaseRequest(
        ProductLevel3InventoryUtil.generateInventoryBaseRequest(ActionKeyEnum.UPDATE_WEB_STOCK));
    request.setPickupPointCode(pickupPointCode);
    if (deltaStock < 0) {
      this.productLevel3InventoryRepository
          .updateStockDecrease(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    } else {
      apiErrorCode = this.productLevel3InventoryRepository
          .updateStockIncreaseV2(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode updateStockForItemPickupPoint(String businessPartnerCode, String gdnSku,
    String pickupPointCode, Integer deltaStock, Integer deltaQuota, boolean updatePreOrderQuota) throws Exception {
    if (!ProductLevel3InventoryUtil.shouldProceedWithUpdate(deltaStock, deltaQuota, updatePreOrderQuota)) {
      return null;
    }

    WebInventoryUpdateStockRequestV2DTO
        request = ProductLevel3InventoryUtil.buildUpdateRequest(businessPartnerCode, gdnSku,
        pickupPointCode, deltaStock, deltaQuota, updatePreOrderQuota);
    
    if (ProductLevel3InventoryUtil.isStockDecrease(deltaStock) || ProductLevel3InventoryUtil.isStockDecrease(deltaQuota)) {
      this.productLevel3InventoryRepository
        .updateStockDecrease(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
      return null;
    } else {
      return this.productLevel3InventoryRepository
        .updateStockIncreaseV2(ProductLevel3InventoryUtil.generateMandatoryRequestParam(), request);
    }
  }


  @Override
  public void deleteByItemSkuAndPickupPointCode(
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = ProductLevel3InventoryUtil.generateMandatoryRequestParam();
    productLevel3InventoryRepository.deleteByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getRequestId(), webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS);
  }

  @Override
  public ApiErrorCode updateOrInsertStock(UpdateOrInsertStockVo updateOrInsertStockVo) throws Exception {
    try {
      ApiErrorCode apiErrorCode = updateStockForItemPickupPoint(updateOrInsertStockVo.getBusinessPartnerCode(),
          updateOrInsertStockVo.getItemSku(), updateOrInsertStockVo.getPickupPointCode(),
          updateOrInsertStockVo.getStock(), updateOrInsertStockVo.getInitialPreOrderQuota(),
          updateOrInsertStockVo.isUpdatePreOrderQuota());
      return apiErrorCode;
    } catch (Exception e) {
      log.error("Error while updating data to inventory. request : {} ", updateOrInsertStockVo, e);
      if (insertInventoryEnabledOnFailedUpdate && e.getMessage()
          .contains(Constants.INVENTORY_NOT_FOUND_ERROR_MESSAGE)) {
        ProductLevel3Inventory productLevel3Inventory =
            RequestHelper.getInventoryInsertRequest(updateOrInsertStockVo.getProfileResponse(),
                updateOrInsertStockVo.getBusinessPartnerCode(), updateOrInsertStockVo.getItemCode(),
                updateOrInsertStockVo.getProductSku(), updateOrInsertStockVo.getItemSku(),
                updateOrInsertStockVo.getPickupPointCode(), updateOrInsertStockVo.getStock(),
                updateOrInsertStockVo.getMinimumStock(), updateOrInsertStockVo.isSyncStock(),
                updateOrInsertStockVo.isFbbActive(), mppForWhEnabled, faasFeatureSwitch,
                updateOrInsertStockVo.getInitialPreOrderQuota(),
                updateOrInsertStockVo.getPreOrderDate(),
                updateOrInsertStockVo.isUpdatePreOrderQuota(), updateOrInsertStockVo.isDistributionPickupPoint());
        insertInventory(Arrays.asList(productLevel3Inventory));
        return null;
      } else {
        throw e;
      }
    }
  }

  @Override
  public void updateSyncStockOrInsertStock(SyncStockUpdateOrInsertVo syncStockUpdateVo) throws Exception {
    try {
      updateSyncStockByBusinessPartnerCodeAndGdnSku(
          syncStockUpdateVo.getUpdateOrInsertStockVo().getBusinessPartnerCode(),
          syncStockUpdateVo.getUpdateOrInsertStockVo().getItemSku(),
          syncStockUpdateVo.getUpdateOrInsertStockVo().isSyncStock(),
          syncStockUpdateVo.getPickupPointSyncStockDtoList());
    } catch (Exception e) {
      log.error("Error while updating sync stock in x-inventory : request : {}, message : {} ", syncStockUpdateVo,
          e.getMessage(), e);
      if (insertInventoryEnabledOnFailedSyncStockUpdate && e.getMessage()
          .contains(Constants.INVENTORY_NOT_FOUND_ERROR_MESSAGE)) {
        ProductLevel3Inventory productLevel3Inventory = RequestHelper.getInventoryInsertRequest(
            syncStockUpdateVo.getUpdateOrInsertStockVo().getProfileResponse(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getBusinessPartnerCode(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getItemCode(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getProductSku(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getItemSku(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getPickupPointCode(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getStock(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().getMinimumStock(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().isSyncStock(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().isFbbActive(), mppForWhEnabled,
            faasFeatureSwitch,
            syncStockUpdateVo.getUpdateOrInsertStockVo().getInitialPreOrderQuota(), null,
            syncStockUpdateVo.getUpdateOrInsertStockVo().isUpdatePreOrderQuota(),
            syncStockUpdateVo.getUpdateOrInsertStockVo().isDistributionPickupPoint());
        insertInventory(Arrays.asList(productLevel3Inventory));
      } else {
        throw e;
      }
    }
  }

  @Override
  public WebInventoryUpdatePickupPointResponseDTO updatePickupPointInInventory(String businessPartnerCode,
      List<InventoryUpsertModel> inventoryUpsertModels) throws Exception {
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    List<ProductLevel3Inventory> productLevel3InventoryList =
        RequestHelper.toProductLevel3InventoryList(profileResponse, inventoryUpsertModels, mppForWhEnabled,
            faasFeatureSwitch);
    insertInventory(productLevel3InventoryList);
    return new WebInventoryUpdatePickupPointResponseDTO();
  }

}
