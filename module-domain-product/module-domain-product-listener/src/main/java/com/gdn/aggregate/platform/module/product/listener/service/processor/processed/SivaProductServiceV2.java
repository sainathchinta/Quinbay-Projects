package com.gdn.aggregate.platform.module.product.listener.service.processor.processed;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaProductRepository;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

@Component
public class SivaProductServiceV2 {

  @Autowired
  private SivaProductRepository sivaProductRepository;

  @Autowired
  private PublisherService publisherService;

  @Autowired
  private SchedulerHelper schedulerHelper;

  public static final String SIVA_PRODUCT_COMBINED_UPSERT = "sivaProductCombinedUpsert";

  public List<SivaProduct> findAllByIds(Set<String> ids) {
    return sivaProductRepository.findByIdIn(ids);
  }

  public SivaProduct getExistingSivaProduct(String id, List<SivaProduct> allSivaProducts) {
    return Optional.ofNullable(allSivaProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(product -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(id))
        .findFirst()
        .orElse(null);
  }

  public Mono<Boolean> publishCombinedSivaProductUpsertModel(SaveParam saveParam, String productSku,
    String topic, SivaProduct sivaProduct) {
    SivaProductCombinedUpsertEventModel sivaProductCombinedUpsertEventModel =
      new SivaProductCombinedUpsertEventModel();
    sivaProductCombinedUpsertEventModel.setEventTrigger(topic);
    sivaProductCombinedUpsertEventModel.setSivaProduct(sivaProduct);
    sivaProductCombinedUpsertEventModel.setSaveParam(saveParam);
    sivaProductCombinedUpsertEventModel.setId(productSku);
    return publisherService.publishSivaProductUpsertCombinedEvent(
      sivaProductCombinedUpsertEventModel).map(Boolean.TRUE::equals);
  }

}
