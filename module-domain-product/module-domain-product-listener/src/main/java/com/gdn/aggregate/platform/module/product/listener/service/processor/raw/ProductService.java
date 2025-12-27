package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ProductRepository;
import org.springframework.util.CollectionUtils;
import reactor.core.publisher.Mono;

@Component
public class ProductService {

  private static final String SAVE_COMMAND = "saveProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private MasterDataProductService masterDataProductService;

  /*Save*/
  public Mono<Boolean> save(Product product, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.PRODUCT)
            .domain(product)
            .clazz(Product.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(product,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(product,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(Product product, SaveParam saveParam) {
    Optional.ofNullable(product)
        .ifPresent(val -> {
          val.setId(val.toId());
          val = completingMasterCatalog(val);
          val = completingMasterDataProduct(val);
        });
  }

  private Product completingMasterCatalog(Product product) {
    Optional.ofNullable(product)
        .filter(Product::isSync)
        .map(Product::getProductCode)
        .ifPresent(productCode ->
            Optional.ofNullable(masterDataService.getExistingMasterData(productCode))
                .map(MasterData::getProductCategories)
                .filter(productCategories -> !CollectionUtils.isEmpty(productCategories))
                .map(MainUtil::toListFirstData)
                .map(MasterData.ProductCategoryDomainEventModel::getCategory)
                .map(this::toMasterCatalog)
                .ifPresent(product::setMasterCatalog));
    return product;
  }

  private String toCatalogCode(MasterData.CategoryDomainEventModel categoryDomainEventModel) {
    return Optional.ofNullable(categoryDomainEventModel)
        .map(MasterData.CategoryDomainEventModel::getCatalog)
        .map(MasterData.CatalogDomainEventModel::getCatalogCode)
        .orElse(null);
  }

  private Product.Category toCategory(MasterData.CategoryDomainEventModel categoryDomainEventModel) {
    return Optional.ofNullable(categoryDomainEventModel)
        .map(ctg -> Product.Category.builder()
            .categoryCode(ctg.getCategoryCode())
            .catgroupId(ctg.getId())
            .sequence(ctg.getSequence())
            .build())
        .orElse(null);
  }

  private Product.MasterCatalog toMasterCatalog(MasterData.CategoryDomainEventModel categoryDomainEventModel) {
    return Optional.ofNullable(categoryDomainEventModel)
        .map(ctg -> Product.MasterCatalog.builder()
            .catalogCode(toCatalogCode(ctg))
            .category(toCategory(ctg))
            .build())
        .orElse(null);
  }

  private MasterDataProduct getCompleteMasterDataProduct(Product product, MasterDataProduct masterDataProduct) {
    if (isProductAlreadyHaveNewestMasterDataProduct(product,masterDataProduct)) {
      return product.getMasterDataProduct();
    }
    if (product.isSync()) {
      return masterDataProduct;
    } else {
      MasterDataProduct newMdp = MainUtil.getOrDefault(product.getMasterDataProduct(),new MasterDataProduct());
      newMdp.setMasterDataProductImages(masterDataProduct.getMasterDataProductImages());
      return newMdp;
    }
  }

  private boolean isProductAlreadyHaveNewestMasterDataProduct(Product product, MasterDataProduct masterDataProduct) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(mdp -> masterDataProduct.getTimestamp() < mdp.getTimestamp())
        .orElseGet(() -> false);
  }

  private Product completingMasterDataProduct(Product product) {
    Optional.ofNullable(product)
        .map(prd -> masterDataProductService.getExistingMasterDataProduct(prd.getProductCode()))
        .map(mdp -> getCompleteMasterDataProduct(product,mdp))
        .ifPresent(product::setMasterDataProduct);
    return product;
  }
  /*End of Save*/

  /*Getters*/
  public Product getExistingProduct(String id) {
    return Optional.ofNullable(id)
        .flatMap(productRepository::findById)
        .orElse(null);
  }

  public List<String> getProductSkusByMerchantCode(String merchantCode) {
    return Optional.ofNullable(merchantCode)
        .map(mhtCode -> {
          try(Stream<Product> productSkuStream = productRepository.getProductSkusByMerchantCode(mhtCode)) {
            return productSkuStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(prd -> MainUtil.toNotNullString(prd.toId())))
                .map(Product::getProductSku)
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<Product> getProductsByProductCode(String productCode) {
    return Optional.ofNullable(productCode)
        .map(prdCode -> {
          try(Stream<Product> productStream = productRepository.streamAllByProductCode(prdCode)) {
            return productStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(prd -> MainUtil.toNotNullString(prd.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<Product> getProductsAfterChangeMasterDataProduct(MasterDataProduct masterDataProduct) {
    return Optional.ofNullable(masterDataProduct)
        .map(MasterDataProduct::getId)
        .map(this::getProductsByProductCode)
        .filter(products -> !CollectionUtils.isEmpty(products))
        .map(products -> products.stream()
            .filter(Objects::nonNull)
            .map(product -> {
              product.setMasterDataProduct(getCompleteMasterDataProduct(product,masterDataProduct));
              return product;
            })
            .collect(Collectors.toList()))
        .orElse(new ArrayList<>());
  }
  /*End of Getters*/

}
