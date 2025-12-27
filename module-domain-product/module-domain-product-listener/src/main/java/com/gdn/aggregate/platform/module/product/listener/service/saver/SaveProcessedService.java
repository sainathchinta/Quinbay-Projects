package com.gdn.aggregate.platform.module.product.listener.service.saver;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.properties.BatchProperties;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component("ProductSaveProcessedService")
public class SaveProcessedService {

  @Autowired
  private BatchProperties batchProperties;

  @Autowired
  private SivaProductService sivaProductService;

  @Autowired
  private SivaItemService sivaItemService;

  /*Processed*/

  public Mono<Boolean> saveSivaProducts(List<SivaProduct> sivaProducts, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(sivaProducts)) {
      return MainUtil.failedResult();
    } else if (CollectionUtils.size(sivaProducts)==1) {
      return saveSivaProduct(MainUtil.toListFirstData(sivaProducts),saveParam);
    } else if (batchProperties.getSave().getEnable()) {
      sivaProducts = sivaProducts.stream()
          .filter(Objects::nonNull)
          .peek(sivaProduct -> sivaProductService.setMandatory(sivaProduct,saveParam))
          .collect(Collectors.toList());
      sivaProductService.setSaveParam(saveParam);
      return Flux.fromIterable(sivaProducts)
          .buffer(batchProperties.getSave().getSize())
          .flatMap(vals -> sivaProductService.upsertMultiple(vals,saveParam))
          .reduce(MainUtil::reduce);
    } else {
      return Flux.fromIterable(sivaProducts)
          .filter(Objects::nonNull)
          .flatMap(sivaProduct -> saveSivaProduct(sivaProduct,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveSivaProduct(SivaProduct sivaProduct, SaveParam saveParam) {
    if (Objects.isNull(sivaProduct)) {
      return MainUtil.failedResult();
    } else {
      sivaProductService.setSaveParam(saveParam);
      sivaProductService.setMandatory(sivaProduct, saveParam);
      return sivaProductService.upsert(sivaProduct, saveParam);
    }
  }

  public Mono<Boolean> saveSivaItems(List<SivaItem> sivaItems, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(sivaItems)) {
      return MainUtil.failedResult();
    } else if (CollectionUtils.size(sivaItems)==1) {
      return saveSivaItem(MainUtil.toListFirstData(sivaItems),saveParam);
    } else if (batchProperties.getSave().getEnable()) {
      sivaItems = sivaItems.stream()
          .filter(Objects::nonNull)
          .peek(sivaItem -> sivaItemService.setMandatory(sivaItem,saveParam))
          .collect(Collectors.toList());
      sivaItemService.setSaveParam(saveParam);
      return Flux.fromIterable(sivaItems)
          .buffer(batchProperties.getSave().getSize())
          .flatMap(vals -> sivaItemService.upsertMultiple(vals,saveParam))
          .reduce(MainUtil::reduce);
    } else {
      return Flux.fromIterable(sivaItems)
          .filter(Objects::nonNull)
          .flatMap(sivaItem -> saveSivaItem(sivaItem,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveSivaItem(SivaItem sivaItem, SaveParam saveParam) {
    if (Objects.isNull(sivaItem)) {
      return MainUtil.failedResult();
    } else {
      sivaItemService.setSaveParam(saveParam);
      sivaItemService.setMandatory(sivaItem, saveParam);
      return sivaItemService.upsert(sivaItem, saveParam);
    }
  }

  public Mono<Boolean> deleteSivaItem(SivaItem sivaItem, SaveParam saveParam) {
    if (Objects.isNull(sivaItem)) {
      return MainUtil.failedResult();
    } else {
      sivaItemService.setSaveParam(saveParam);
      sivaItemService.setMandatory(sivaItem, saveParam);
      return sivaItemService.delete(sivaItem, saveParam);
    }
  }

}
