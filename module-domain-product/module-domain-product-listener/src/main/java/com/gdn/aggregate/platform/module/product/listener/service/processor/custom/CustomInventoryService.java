package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomInventoryInfoRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomInventoryPickupPointInfoRepository;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointInventoryService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointServiceV2;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Component("ProductCustomInventoryService")
public class CustomInventoryService {

  @Autowired
  private CustomInventoryPickupPointInfoRepository inventoryPickupPointInfoRepository;

  @Autowired
  private CustomInventoryInfoRepository inventoryInfoRepository;

  @Autowired
  private PickupPointInventoryService pickupPointInventoryService;

  @Autowired
  PickupPointServiceV2 pickupPointServiceV2;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  public Stock getStockByL5(String itemSku, String pickupPointCode, PickupPoint pickupPoint) {
    if (skipStockFetchFromInventoryModule) {
      return getStockByL5FromPickupPointInventory(itemSku, pickupPointCode, pickupPoint);
    } else {
      return getStockByL5FromInventoryModule(itemSku, pickupPointCode);
    }
  }

  private Stock getStockByL5FromPickupPointInventory(String itemSku, String pickupPointCode,
    PickupPoint pickupPoint) {
    String pickupPointId = ModuleProductUtil.toPickupPointId(itemSku, pickupPointCode);
    PickupPointInventory pickupPointInventory = Optional.ofNullable(
        pickupPointInventoryService.getExistingPickupPointInventory(pickupPointId))
      .orElseGet(() -> ModuleProductUtil.toPickupPointInventoryFromPickupPoint(
        pickupPointServiceV2.getExistingPickupPoint(pickupPointId)));
    
    if (Objects.nonNull(pickupPointInventory)) {
      return ModuleProductUtil.buildBasicStock(itemSku, pickupPointCode, pickupPointInventory);
    }
    return ModuleProductUtil.initializeStockData(itemSku, pickupPointCode, pickupPoint);
  }

  private Stock getStockByL5FromInventoryModule(String itemSku, String pickupPointCode) {
    return Optional.ofNullable(getStockByL5FromInventoryPickupPointInfo(itemSku, pickupPointCode))
      .orElseGet(() -> getStockByL5FromInventoryInfo(itemSku, pickupPointCode));
  }

  private Stock getStockByL5FromInventoryPickupPointInfo(String itemSku, String pickupPointCode) {
    String pickupPointId = ModuleProductUtil.toPickupPointId(itemSku, pickupPointCode);
    return getStockFromInventoryModule(itemSku, pickupPointCode, pickupPointId);
  }

  private Stock getStockFromInventoryModule(String itemSku, String pickupPointCode, String pickupPointId) {
    return Optional.ofNullable(pickupPointId).flatMap(inventoryPickupPointInfoRepository::findById)
      .filter(val -> !val.isMarkForDelete()).map(
        val -> Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode)
          .quota(val.getOriginalStock()).remaining(val.getAvailableStock())
          .warehouse(ModuleProductUtil.isWarehouse(ModuleProductUtil.toPickupPointType(val)))
          .status(ModuleProductUtil.convertStockStatus(val.getAvailableStock())).exists(true)
          .build()).orElse(null);
  }

  private Stock getStockByL5FromInventoryInfo(String itemSku, String pickupPointCode) {
    return Optional.ofNullable(itemSku)
        .map(iSku -> inventoryInfoRepository.findFirstByItemSkuAndType(iSku,InventoryType.TYPE_ONLINE_MERCHANT))
        .filter(inventoryInfo -> CollectionUtils.isNotEmpty(inventoryInfo.getStockInformations()))
        .map(inventoryInfo -> {
          List<CustomInventoryInfo.StockInformation> stockInformations = inventoryInfo.getStockInformations();
          Stock result = ModuleProductUtil.initializeStock(itemSku,pickupPointCode);
          int totalOriginalStock = stockInformations.stream()
              .filter(Objects::nonNull)
              .map(CustomInventoryInfo.StockInformation::getStockInformationDetails)
              .filter(CollectionUtils::isNotEmpty)
              .flatMap(Collection::stream)
              .filter(Objects::nonNull)
              .filter(val -> ModuleProductUtil.isPickupPointCodeSame(pickupPointCode,val.getPickupPointCode()))
              .map(CustomInventoryInfo.StockInformationDetail::getOriginalStock)
              .mapToInt(Integer::intValue)
              .sum();
          int totalAvailableStock = stockInformations.stream()
              .filter(Objects::nonNull)
              .map(CustomInventoryInfo.StockInformation::getStockInformationDetails)
              .filter(CollectionUtils::isNotEmpty)
              .flatMap(Collection::stream)
              .filter(Objects::nonNull)
              .filter(val -> ModuleProductUtil.isPickupPointCodeSame(pickupPointCode,val.getPickupPointCode()))
              .map(CustomInventoryInfo.StockInformationDetail::getAvailableStock)
              .mapToInt(Integer::intValue)
              .sum();
          result.setItemSku(itemSku);
          result.setPickupPointCode(pickupPointCode);
          result.setQuota(result.getQuota() + totalOriginalStock);
          result.setRemaining(result.getRemaining() + totalAvailableStock);
          result.setWarehouse(ModuleProductUtil.isWarehouse(inventoryInfo.getType()));
          result.setStatus(ModuleProductUtil.convertStockStatus(result.getRemaining()));
          result.setExists(true);
          return result;
        })
        .orElseGet(() -> ModuleProductUtil.initializeStock(itemSku,pickupPointCode));
  }

}
