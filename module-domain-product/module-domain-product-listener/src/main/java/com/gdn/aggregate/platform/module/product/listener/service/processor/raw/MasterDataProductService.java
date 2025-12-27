package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataProductRepository;
import reactor.core.publisher.Mono;

@Component
public class MasterDataProductService {

  private static final String SAVE_COMMAND = "saveMasterDataProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private MasterDataProductRepository masterDataProductRepository;

  /*Save*/
  public Mono<Boolean> save(MasterDataProduct masterDataProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.MASTER_DATA_PRODUCT)
            .domain(masterDataProduct)
            .clazz(MasterDataProduct.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(masterDataProduct,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(masterDataProduct,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(MasterDataProduct masterDataProduct) {
    Optional.ofNullable(masterDataProduct)
        .ifPresent(val -> {
          val.setId(val.toId());
        });
  }
  /*End of Save*/

  /*Getters*/
  public MasterDataProduct getExistingMasterDataProduct(String id) {
    return Optional.ofNullable(id)
        .flatMap(masterDataProductRepository::findById)
        .orElse(null);
  }

  public MasterDataProduct toMasterDataProduct(MasterData masterData) {
    return Optional.ofNullable(masterData)
        .filter(val -> Objects.nonNull(val.getProductCode()))
        .map(val -> {
          MasterDataProduct result =
              MasterDataProduct.builder()
                  .timestamp(val.getTimestamp())
                  .productCode(val.getProductCode())
                  .brand(val.getBrand())
                  .brandLogoUrl(val.getBrandLogoUrl())
                  .shippingWeight(val.getShippingWeight())
                  .specificationDetail(val.getSpecificationDetail())
                  .productName(val.getName())
                  .description(MainUtil.toString(val.getDescription()))
                  .longDescription(MainUtil.toString(val.getDescription()))
                  .uniqueSellingPoint(val.getUniqueSellingPoint())
                  .activated(val.isActivated())
                  .viewable(val.isViewable())
                  .productStory(val.getProductStory())
                  .uom(val.getUom())
                  .newData(val.isNewProduct())
                  .markForDelete(toMasterDataMarkForDelete(val.getProductItems()))
                  .masterDataProductImages(toMasterDataProductImages(val.getImages(),val.getProductCode()))
                  .masterDataProductAttributes(toMasterDataProductAttributes(val.getProductAttributes()))
                  .url(val.getUrl())
                  .length(val.getLength())
                  .width(val.getWidth())
                  .height(val.getHeight())
                  .weight(val.getWeight())
                  .build();
          result.setId(result.toId());
          return result;
        })
        .orElse(null);
  }

  private boolean toMasterDataMarkForDelete(List<MasterData.ProductItemDomainEventModel> productItems) {
    return Optional.ofNullable(productItems)
        .orElseGet(ArrayList::new)
        .stream()
        .allMatch(MasterData.ProductItemDomainEventModel::isMarkForDelete);
  }

  private List<MasterDataProduct.MasterDataProductImage> toMasterDataProductImages(List<MasterData.ImageDomainEventModel> images, String productCode) {
    return Optional.ofNullable(images)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(image -> toMasterDataProductImage(image,productCode))
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(img -> MainUtil.toNotNullString(img.getLocationPath())))
        .collect(Collectors.toList());
  }

  private MasterDataProduct.MasterDataProductImage toMasterDataProductImage(MasterData.ImageDomainEventModel image, String productCode) {
    return Optional.ofNullable(image)
        .map(val -> MasterDataProduct.MasterDataProductImage.builder()
            .mainImage(val.isMainImage())
            .locationPath(val.getLocationPath())
            .productCode(productCode)
            .sequence(ModuleProductUtil.toSafeSequence(val.getSequence()))
            .build())
        .orElse(null);
  }

  private List<MasterDataProduct.MasterDataProductAttribute> toMasterDataProductAttributes(List<MasterData.ProductAttributeDomainEventModel> productAttributes) {
    return Optional.ofNullable(productAttributes)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(this::toMasterDataProductAttribute)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(prAC -> MainUtil.toNotNullString(getAttributeCode(prAC))))
        .collect(Collectors.toList());
  }

  private String getAttributeCode(MasterDataProduct.MasterDataProductAttribute masterDataProductAttribute) {
    return Optional.ofNullable(masterDataProductAttribute)
        .map(MasterDataProduct.MasterDataProductAttribute::getMasterDataAttribute)
        .map(MasterDataProduct.MasterDataAttribute::getAttributeCode)
        .orElse(null);
  }

  private MasterDataProduct.MasterDataProductAttribute toMasterDataProductAttribute(MasterData.ProductAttributeDomainEventModel productAttribute) {
    return Optional.ofNullable(productAttribute)
        .map(val -> MasterDataProduct.MasterDataProductAttribute.builder()
            .ownByProductItem(val.isOwnByProductItem())
            .masterDataAttribute(toMasterDataAttribute(val.getAttribute()))
            .sequence(ModuleProductUtil.toSafeSequence(val.getSequence()))
            .masterDataProductAttributeValues(toMasterDataProductAttributeValues(val.getProductAttributeValues()))
            .build())
        .orElse(null);
  }

  private MasterDataProduct.MasterDataAttribute toMasterDataAttribute(MasterData.AttributeDomainEventModel attribute) {
    return Optional.ofNullable(attribute)
        .map(val -> MasterDataProduct.MasterDataAttribute.builder()
            .attributeCode(val.getAttributeCode())
            .attributeType(val.getAttributeType())
            .attributeName(val.getName())
            .description(MainUtil.toString(val.getDescription()))
            .skuValue(val.isSkuValue())
            .build())
        .orElse(null);
  }

  private List<MasterDataProduct.MasterDataProductAttributeValue> toMasterDataProductAttributeValues(List<MasterData.ProductAttributeValueDomainEventModel> productAttributeValues) {
    return Optional.ofNullable(productAttributeValues)
        .orElseGet(ArrayList::new)
        .stream()
        .map(this::toMasterDataProductAttributeValue)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(prdAV -> MainUtil.toNotNullString(prdAV.getAllowedAttributeValueCode())))
        .collect(Collectors.toList());
  }

  private MasterDataProduct.MasterDataProductAttributeValue toMasterDataProductAttributeValue(MasterData.ProductAttributeValueDomainEventModel productAttributeValue) {
    return Optional.ofNullable(productAttributeValue)
        .map(val -> MasterDataProduct.MasterDataProductAttributeValue.builder()
            .descriptiveAttributeValue(val.getDescriptiveAttributeValue())
            .descriptiveAttributeValueType(val.getDescriptiveAttributeValueType())
            .predefinedAllowedAttributeValueCode(toPredefinedAllowedAttributeValue(val.getPredefinedAllowedAttributeValue()))
            .allowedAttributeValue(toMasterDataAllowedAttributeValue(val.getAllowedAttributeValue()))
            .build())
        .orElse(null);
  }

  private String toPredefinedAllowedAttributeValue(MasterData.PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttribute) {
    return Optional.ofNullable(predefinedAllowedAttribute)
        .map(MasterData.PredefinedAllowedAttributeValueDomainEventModel::getValue)
        .orElse(null);
  }

  private MasterDataProduct.MasterDataAllowedAttributeValue toMasterDataAllowedAttributeValue(MasterData.AllowedAttributeValueDomainEventModel allowedAttributeValue) {
    return Optional.ofNullable(allowedAttributeValue)
        .map(val -> MasterDataProduct.MasterDataAllowedAttributeValue.builder()
            .allowedAttributeValueCode(val.getAllowedAttributeCode())
            .value(val.getValue())
            .sequence(ModuleProductUtil.toSafeSequence(val.getSequence()))
            .build())
        .orElse(null);
  }
  /*End of Getters*/

}
