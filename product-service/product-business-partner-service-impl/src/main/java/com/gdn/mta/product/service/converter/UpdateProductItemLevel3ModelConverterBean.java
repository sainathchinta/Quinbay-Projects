package com.gdn.mta.product.service.converter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class UpdateProductItemLevel3ModelConverterBean extends BaseObjectConverter implements
    UpdateProductItemLevel3ModelConverter {

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public UpdateProductItemLevel3Model convertFromProductLevel3Summary(ProductLevel3Summary product) {
    checkSourceObject(product);
    UpdateProductItemLevel3Model result =
        UpdateProductItemLevel3Model.builder()
            .availableStockLevel2(product.getAvailableStockLevel2())
            .synchronizeStock(product.getSynchronizeStock())
            .off2OnActiveFlag(product.getOff2OnActiveFlag())
            .merchantSku(product.getMerchantSku())
            .cnc(product.isCncActive())
            .lateFulfillment(!product.getLateFulfillment()).build();

    if (CollectionUtils.isNotEmpty(product.getPrices())) {
      populateFromProductLevel3Price(result, product.getPrices().get(0));
    }

    if (CollectionUtils.isNotEmpty(product.getViewConfigs())) {
      populateFromProductLevel3ViewConfig(result, product.getViewConfigs().stream()
          .filter(productLevel3ViewConfig -> Constants.DEFAULT.equals(productLevel3ViewConfig.getChannelId()))
          .findFirst().orElse(new ProductLevel3ViewConfig()));
      if (cncForWarehouseFeatureSwitch) {
        populateFromCncProductLevel3ViewConfig(result, product.getViewConfigs().stream()
            .filter(productLevel3ViewConfig -> Constants.CNC_CHANNEL.equals(productLevel3ViewConfig.getChannelId())).findFirst()
            .orElse(new ProductLevel3ViewConfig()));
      }
    }
    if (Objects.nonNull(product.getMinimumStockLevel2())) {
      result.setMinimumStock(String.valueOf(product.getMinimumStockLevel2()));
    }
    result.setPickupPointCode(product.getPickupPointCode());
    result.setOff2OnActiveFlag(product.getOff2OnActiveFlag());
    if (Objects.nonNull(product.getWholesalePriceActivated())) {
      result.setWholesalePriceActivated(
          Boolean.TRUE.equals(product.getWholesalePriceActivated()) ? Constants.ACTIVE : Constants.INACTIVE);
    } else {
      result.setWholesalePriceActivated(Constants.INACTIVE);
    }
    return result;
  }

  @Override
  public UpdateProductItemLevel3Model convertFromProductLevel3(ProductLevel3 product, boolean mapWithAttributeCode) {
    checkSourceObject(product);
    String productType = null;
    if (Constants.REGULAR.equals(product.getProductType())) {
      productType = ProductType.REGULAR.getDescription();
    } else if (Constants.BIG_PRODUCT.equals(product.getProductType())) {
      productType = ProductType.BIG_PRODUCT.getDescription();
    } else if (Constants.BOPIS.equals(product.getProductType())) {
      productType = ProductType.BOPIS.getDescription();
    }
    UpdateProductItemLevel3Model result =
        UpdateProductItemLevel3Model.builder().productName(product.getProductName())
            .description(product.getDescription()).productType(productType).build();
    List<String> attributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(product.getAttributes())) {
      for (ProductLevel3Attribute productLevel3Attribute : product.getAttributes()) {
        if (mapWithAttributeCode) {
          attributes.add(new StringBuilder().append(productLevel3Attribute.getAttributeCode()).append(Constants.DELIMITER)
              .append(productLevel3Attribute.getValues().get(0)).toString());
        } else {
          attributes.add(new StringBuilder().append(productLevel3Attribute.getAttributeName()).append(Constants.DELIMITER)
              .append(productLevel3Attribute.getValues().get(0)).toString());
        }
      }
    }
    result.setAttributesMap(attributes.toArray(new String[attributes.size()]));
    result.setUrlVideo(product.getUrl());
    result.setUsp(product.getUniqueSellingPoint());
    result.setSizeChartCode(product.getSizeChartCode());
    if(Objects.nonNull(product.getInstallationRequired())){
      result.setInstallationFlag(String.valueOf(product.getInstallationRequired()));
    }
    if (product.isNeedCorrection()) {
      setDimensionAndDangerousGoodsLevel(product, result);
    }
    if (CollectionUtils.isNotEmpty(product.getItems())) {
      ProductItemLevel3 productItem = product.getItems().get(0);
      populateFromProductItemLevel3(result, productItem);

      if (CollectionUtils.isNotEmpty(productItem.getViewConfigs())) {
        populateFromProductLevel3ViewConfig(result, productItem.getViewConfigs().get(0));
      }
    }
    return result;
  }

  private static void setDimensionAndDangerousGoodsLevel(ProductLevel3 product, UpdateProductItemLevel3Model result) {
    if (Objects.nonNull(product.getLength())) {
      result.setLength(product.getLength());
    }
    if (Objects.nonNull(product.getWidth())) {
      result.setWidth(product.getWidth());
    }
    if (Objects.nonNull(product.getHeight())) {
      result.setHeight(product.getHeight());
    }
    if (Objects.nonNull(product.getWeight())) {
      result.setWeight(product.getWeight());
    }
    if (Objects.nonNull(product.getDangerousGoodsLevel())) {
      result.setDangerousGoodsLevel(product.getDangerousGoodsLevel());
    }
  }

  private void populateFromProductLevel3Price(UpdateProductItemLevel3Model result,
      ProductLevel3Price productPrice) {
    checkSourceObject(productPrice);
    result.setPrice(productPrice.getPrice());
    result.setSalePrice(productPrice.getSalePrice());
  }

  private void populateFromProductLevel3ViewConfig(UpdateProductItemLevel3Model result,
      ProductLevel3ViewConfig productViewConfig) {
    checkSourceObject(productViewConfig);
    result.setDisplayable(productViewConfig.getDisplay());
    result.setBuyable(productViewConfig.getBuyable());
  }

  private void populateFromCncProductLevel3ViewConfig(UpdateProductItemLevel3Model result,
      ProductLevel3ViewConfig productViewConfig) {
    checkSourceObject(productViewConfig);
    result.setCncDisplayable(productViewConfig.getDisplay());
    result.setCncBuyable(productViewConfig.getBuyable());
  }

  private void populateFromProductItemLevel3(UpdateProductItemLevel3Model result,
      ProductItemLevel3 productItem) {
    checkSourceObject(productItem);
    result.setAvailableStockLevel2(productItem.getAvailableStockLevel2());
    result.setSynchronizeStock(productItem.getSynchronizeStock());
    result.setOff2OnActiveFlag(productItem.getOff2OnActiveFlag());
    result.setMerchantSku(productItem.getMerchantSku());
    result.setPickupPointCode(productItem.getPickupPointCode());
    result.setWeight(productItem.getWeight());
    result.setHeight(productItem.getHeight());
    result.setLength(productItem.getLength());
    result.setWidth(productItem.getWidth());
    result.setDangerousGoodsLevel(productItem.getDangerousGoodsLevel());
    result.setMinimumStock(productItem.getMinimumStock().toString());
    if (Objects.nonNull(productItem.getWholesalePriceActivated())) {
      result.setWholesalePriceActivated(String.valueOf(productItem.getWholesalePriceActivated()));
    }
    if (CollectionUtils.isNotEmpty(productItem.getPrices())) {
      result.setPrice(productItem.getPrices().get(0).getPrice());
      result.setSalePrice(productItem.getPrices().get(0).getSalePrice());
    }
  }

  private void populateFromProductResponse(UpdateProductItemLevel3Model result,
      ProductResponse productResponse) {
    checkSourceObject(productResponse);
    MasterDataProductDTO masterDataProduct = productResponse.getMasterDataProduct();
    checkSourceObject(masterDataProduct);
    result.setProductName(productResponse.getMasterDataProduct().getProductName());
    result.setDescription(productResponse.getMasterDataProduct().getDescription());
    result.setUsp(productResponse.getMasterDataProduct().getUniqueSellingPoint());
    result.setProductType(productResponse.getProductType().getDescription());
  }
  
  private void populateFromItemResponse(UpdateProductItemLevel3Model result, ItemResponse itemResponse) {
    checkSourceObject(itemResponse);
    if (CollectionUtils.isNotEmpty(itemResponse.getPrice())) {
      for (PriceDTO itemPrice : itemResponse.getPrice()) {
        if (itemPrice.getChannel().equals(ChannelName.DEFAULT.name())) {
          result.setPrice(itemPrice.getListPrice());
          result.setSalePrice(itemPrice.getOfferPrice());
        }
      }
    }
    
    if (CollectionUtils.isNotEmpty(itemResponse.getItemViewConfigs())) {
      for (ItemViewConfigDTO itemViewConfig : itemResponse.getItemViewConfigs()) {
        if (itemViewConfig.getChannel().equals(ChannelName.DEFAULT.name())) {
          result.setDisplayable(itemViewConfig.isDiscoverable());
          result.setBuyable(itemViewConfig.isBuyable());
        }
      }
    }
    
    result.setSynchronizeContent(itemResponse.isSynchronized());
    result.setOff2OnActiveFlag(itemResponse.getOff2OnChannelActive());
    result.setWholesalePriceActivated(Optional.ofNullable(itemResponse.getWholesalePriceActivated()).orElse(false) ?
        Constants.ACTIVE : Constants.INACTIVE);
  }

  @Override
  public UpdateProductItemLevel3Model convertFromProductAndItemsResponse(
      ProductAndItemsResponse request) {
    checkSourceObject(request);
    UpdateProductItemLevel3Model result = new UpdateProductItemLevel3Model();
    populateFromProductResponse(result, request.getProduct());
    populateFromItemResponse(result, request.getItems().get(0));
    return result;
  }

  @Override
  public UpdateProductItemLevel3Model convertFromProductLevel3Inventory(
      ProductLevel3Inventory request) {
    checkSourceObject(request);
    UpdateProductItemLevel3Model result = new UpdateProductItemLevel3Model();
    result.setAvailableStockLevel2(request.getWebAvailable());
    result.setMinimumStock(String.valueOf(request.getWebMinAlert()));
    return result;
  }

  @Override
  public UpdateProductItemLevel3Model convertFromProductLevel3Detail(ProductLevel3DetailResponse request, boolean mapWithAttributeCode) {
    checkSourceObject(request);
    String productType = null;
    if (request.getProductType() == ProductType.REGULAR.getCode()) {
      productType = ProductType.REGULAR.getDescription();
    } else if (request.getProductType() == ProductType.BIG_PRODUCT.getCode()) {
      productType = ProductType.BIG_PRODUCT.getDescription();
    } else if (request.getProductType() == ProductType.BOPIS.getCode()) {
      productType = ProductType.BOPIS.getDescription();
    }
    List<String> attributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(request.getAttributes())) {
      for (ProductLevel3AttributeResponse productLevel3Attribute : request.getAttributes()) {
        String attributeValue = CollectionUtils.isNotEmpty(productLevel3Attribute.getValues()) ?
          productLevel3Attribute.getValues().get(0) : Constants.DELIMITER_DASH;
        if (mapWithAttributeCode) {
          attributes.add(new StringBuilder().append(productLevel3Attribute.getAttributeCode()).append(Constants.DELIMITER)
              .append(attributeValue).toString());
        } else {
          attributes.add(new StringBuilder().append(productLevel3Attribute.getAttributeName()).append(Constants.DELIMITER)
              .append(attributeValue).toString());
        }
      }
    }
    UpdateProductItemLevel3Model result =
        UpdateProductItemLevel3Model.builder().productName(request.getProductName())
            .width(request.getWidth()).height(request.getHeight()).length(request.getLength())
            .weight(request.getWeight()).description(request.getDescription()).productType(productType)
            .attributesMap(attributes.toArray(new String[attributes.size()])).urlVideo(request.getUrl())
            .usp(request.getUniqueSellingPoint()).off2OnActiveFlag(request.isOff2OnChannelActive())
            .freeSample(request.isFreeSample()).sizeChartCode(request.getSizeChartCode())
            .sizeChartChanged(request.isSizeChartChanged()).build();
    return result;
  }

  @Override
  public UpdateProductItemLevel3Model convertFromItemPickupPointListingResponse(
      ItemPickupPointListingResponse itemPickupPointListingResponse, Map<String, Boolean> addingWholeSale1stTimeL5s) {
    UpdateProductItemLevel3Model updateProductItemLevel3Model = new UpdateProductItemLevel3Model();
    updateProductItemLevel3Model.setSalePrice(itemPickupPointListingResponse.getPrices().get(0).getSalePrice());
    updateProductItemLevel3Model.setPrice(itemPickupPointListingResponse.getPrices().get(0).getPrice());
    updateProductItemLevel3Model.setBuyable(itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isBuyable());
    updateProductItemLevel3Model.setDisplayable(itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isDisplay());
    updateProductItemLevel3Model.setCncBuyable(itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.CNC.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isBuyable());
    updateProductItemLevel3Model.setCncDisplayable(itemPickupPointListingResponse.getViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.CNC.name().equals(itemViewConfig.getChannelId())).findFirst()
        .orElse(new ViewConfigResponse()).isDisplay());
    updateProductItemLevel3Model.setPickupPointCode(itemPickupPointListingResponse.getPickUpPointCode());
    if (addingWholeSale1stTimeL5s.containsKey(
        itemPickupPointListingResponse.getItemSku() + Constants.HYPHEN + itemPickupPointListingResponse
            .getPickUpPointCode())) {
      updateProductItemLevel3Model.setWholesalePriceActivated(null);
    } else {
      updateProductItemLevel3Model.setWholesalePriceActivated(
          Boolean.TRUE.equals(itemPickupPointListingResponse.getWholesalePriceActivated()) ?
              Constants.ACTIVE :
              Constants.INACTIVE);
    }
    if (!cncForWarehouseFeatureSwitch) {
      updateProductItemLevel3Model.setCnc(itemPickupPointListingResponse.isCncActive());
    }
    if (itemPickupPointListingResponse.isFbbActive()) {
      updateProductItemLevel3Model.setFbbActivated(itemPickupPointListingResponse.isFbbActive());
    } else {
      updateProductItemLevel3Model.setFbbDeactivated(itemPickupPointListingResponse.isFbbActive());
    }
    return updateProductItemLevel3Model;
  }

  @Override
  public UpdateProductItemLevel3Model convertFromItemPickupPoint(ItemPickupPoint itemPickupPoint,
      Set<String> wholeSaleFlagUpdatedL5s, UpdateProductItemLevel3Model savedData,
      Map<String, Boolean> addingWholeSale1stTimeL5s) {
    String L5Code = itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode();
    UpdateProductItemLevel3Model updateProductItemLevel3Model = new UpdateProductItemLevel3Model();
    updateProductItemLevel3Model.setSalePrice(itemPickupPoint.getPrice().iterator().next().getOfferPrice());
    updateProductItemLevel3Model.setPrice(itemPickupPoint.getPrice().iterator().next().getListPrice());
    updateProductItemLevel3Model.setBuyable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannel())).findFirst()
        .orElse(new ItemViewConfig()).isBuyable());
    updateProductItemLevel3Model.setDisplayable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.DEFAULT.name().equals(itemViewConfig.getChannel())).findFirst()
        .orElse(new ItemViewConfig()).isDiscoverable());
    updateProductItemLevel3Model.setCncBuyable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.CNC.name().equals(itemViewConfig.getChannel())).findFirst()
        .orElse(new ItemViewConfig()).isBuyable());
    updateProductItemLevel3Model.setCncDisplayable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> ChannelName.CNC.name().equals(itemViewConfig.getChannel())).findFirst()
        .orElse(new ItemViewConfig()).isDiscoverable());
    updateProductItemLevel3Model.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    // If wholesale flag is updated then save value as opposite to that of saved data
    if (addingWholeSale1stTimeL5s.containsKey(L5Code)) {
      updateProductItemLevel3Model
          .setWholesalePriceActivated(addingWholeSale1stTimeL5s.get(L5Code) ? Constants.ACTIVE : Constants.INACTIVE);
    } else if (wholeSaleFlagUpdatedL5s.contains(L5Code)) {
      updateProductItemLevel3Model.setWholesalePriceActivated(
          Constants.ACTIVE.equals(savedData.getWholesalePriceActivated()) ? Constants.INACTIVE : Constants.ACTIVE);
    }
    if (!cncForWarehouseFeatureSwitch) {
      updateProductItemLevel3Model.setCnc(itemPickupPoint.isCncActive());
    }
    if (itemPickupPoint.isFbbActivated()) {
      updateProductItemLevel3Model.setFbbActivated(itemPickupPoint.isFbbActivated());
    } else {
      updateProductItemLevel3Model.setFbbDeactivated(itemPickupPoint.isFbbActivated());
    }
    return updateProductItemLevel3Model;
  }
}
