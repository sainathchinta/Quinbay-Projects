package com.gdn.aggregate.platform.module.product.listener.service.saver;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.event.ProductElasticSearchDeletionEvent;
import com.gdn.aggregate.platform.module.product.listener.model.util.TerminatedSellerDeletionEventModel;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.PickupPointRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ProductDeletionService {

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private SivaItemRepository sivaItemRepository;

  @Autowired
  private SivaProductRepository sivaProductRepository;

  @Autowired
  private MasterDataProductRepository masterDataProductRepository;

  @Autowired
  private MasterDataItemRepository masterDataItemRepository;

  @Autowired
  private SivaItemService sivaItemService;

  @Autowired
  private SivaProductService sivaProductService;

  @Autowired
  private PublisherService publisherService;

  @Autowired
  private SchedulerHelper schedulerHelper;

  @Value("${permanent-delete.processing.bulk-size:1}")
  private int bulkSize;

  @Value("${permanent-delete.processing.es-deletion-timeout-seconds}")
  private int esDeletionTimeoutSeconds;

  private int getEffectiveBulkSize() {
    return Math.max(1, bulkSize);
  }

  public Mono<Void> performDeletionFromAllDataSources(
    List<TerminatedSellerDeletionEventModel> terminatedSellerDeletionEventModels, String traceId) {
    Map<String, TerminatedSellerDeletionEventModel> skuToModelMap =
      terminatedSellerDeletionEventModels.stream().filter(Objects::nonNull)
        .collect(
          Collectors.toMap(TerminatedSellerDeletionEventModel::getProductSku, Function.identity(),
            (first, second) -> first));
    SaveParam saveParam = SaveParam.builder().traceId(traceId).build();
    if (skuToModelMap.isEmpty()) {
      log.error("No valid productSkus found for deletion, traceId={}", traceId);
      return Mono.empty();
    }
    log.info("Starting deletion process for {} productSku(s): {}, traceId={}",
      skuToModelMap.size(), skuToModelMap.keySet(), traceId);

    Mono<Void> esDeletion = publisherService.publishProductEsDeletionEvent(
        ProductElasticSearchDeletionEvent.builder().productSkus(skuToModelMap.keySet())
          .timestamp(ModuleProductUtil.getCurrentTimestamp()).traceId(traceId).build())
      .timeout(Duration.ofSeconds(esDeletionTimeoutSeconds)).onErrorResume(e -> {
        log.error("ES Deletions failed for productSku(s): {}, traceId: {}, error: {}",
          skuToModelMap.keySet(), traceId, e.getMessage());
        return Mono.just(false);
      }).then();

    Mono<Void> mongoDeletions = Flux.fromIterable(skuToModelMap.entrySet())
      .flatMap(entry -> performMongoDeletions(entry.getKey(), entry.getValue(), saveParam)
        .timeout(Duration.ofSeconds(45))
        .onErrorResume(e -> {
          log.error("Critical error in MongoDB deletions for productSku: {}, traceId: {}",
            entry.getKey(), traceId, e);
          performFallbackDeletion(entry.getKey());
          return Mono.empty();
        }), 2) 
      .then();

    return Mono.when(esDeletion, mongoDeletions)
      .timeout(Duration.ofMinutes(5))
      .onErrorResume(e -> {
        log.error("Global error in deletion process, traceId: {}, error: {}", traceId,
          e.getMessage());
        return Mono.empty();
      });
  }

  private Mono<Void> performMongoDeletions(String productSku, TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel,
    SaveParam saveParam) {
    return Mono.fromCallable(() -> pickupPointRepository.deleteByProductSku(productSku))
      .timeout(Duration.ofSeconds(10))
      .doOnSuccess(count -> log.info("Deleted {} pickup points for productSku: {}", count, productSku))
      .doOnError(e -> log.error("Error deleting pickup points for productSku: {}", productSku, e))
      .onErrorResume(e -> Mono.just(0L))
      .then(Mono.defer(() -> {
        // Process data deletions
        deleteProcessedDataOptimized(productSku, saveParam);
        return Mono.empty();
      }))
      .then(Mono.fromRunnable(() -> deleteRawDataSources(productSku)))
      .timeout(Duration.ofSeconds(10))
      .doOnSuccess(v -> log.info("Successfully deleted raw data for productSku: {}", productSku))
      .doOnError(e -> log.error("Error deleting raw data for productSku: {}", productSku, e))
      .onErrorResume(e -> Mono.empty())
      .then(Mono.fromRunnable(() -> deleteMasterData(terminatedSellerDeletionEventModel)))
      .timeout(Duration.ofSeconds(10))
      .doOnSuccess(v -> log.info("Successfully deleted master data for productSku: {}", productSku))
      .doOnError(e -> log.error("Error deleting master data for productSku: {}", productSku, e))
      .onErrorResume(e -> Mono.empty())
      .then()
      .doOnSuccess(v -> log.info("Successfully completed all deletion processes for productSku: {}", productSku))
      .onErrorResume(error -> {
        log.error("Critical error during deletion process for productSku: {}", productSku, error);
        performFallbackDeletion(productSku);
        return Mono.empty();
      });
  }

  public void deleteProcessedDataOptimized(String productSku, SaveParam saveParam) {
        boolean productDeleted = false;
        boolean itemsDeleted = false;
        try {
            sivaProductRepository.deleteById(productSku);
            productDeleted = true;
            log.info("Successfully deleted product from MongoDB for productSku: {}", productSku);
        } catch (Exception e) {
            log.error("Failed to delete product for productSku: {}, continuing with remaining operations", productSku, e);
        }
        try {
            sivaItemService.performItemDeletionByProductSku(productSku, saveParam);
            itemsDeleted = true;
            log.info("Successfully deleted items from MongoDB for productSku: {}", productSku);
        } catch (Exception e) {
            log.error("Failed to delete items for productSku: {}, continuing with remaining operations", productSku, e);
        }
        if (productDeleted && itemsDeleted) {
            log.info("Successfully completed all MongoDB deletions for productSku: {}", productSku);
        } else {
            log.warn("Partial deletion completed for productSku: {}. Product deleted: {}, Items deleted: {}", 
                productSku, productDeleted, itemsDeleted);
            if (!productDeleted && !itemsDeleted) {
                log.error("Both deletions failed for productSku: {}, performing fallback", productSku);
                performFallbackDeletion(productSku);
            }
        }
  }

  public Mono<Void> performElasticSearchDeletion(String productSku, SaveParam saveParam) {
    Mono<Boolean> sivaProductESDeletion =
      sivaProductService.deleteProductByProductSkuFromElasticsearch(productSku, saveParam)
        .doOnSuccess(success -> {
          if (Boolean.TRUE.equals(success)) {
            log.debug("ES Product deletion successful for productSku: {} ", productSku);
          } else {
            log.error("ES Product deletion returned false for productSku: {} ", productSku);
          }
        });

    Mono<Boolean> sivaItemsESDeletion =
      sivaItemService.deleteByProductSkuFromElasticsearch(productSku, saveParam)
        .doOnSuccess(success -> {
          if (Boolean.TRUE.equals(success)) {
            log.debug("ES Items deletion successful for productSku: {} ", productSku);
          } else {
            log.error("ES Items deletion returned false for productSku: {} ", productSku);
          }
        });

    // Execute ES operations in parallel
    return Mono.zip(sivaProductESDeletion.onErrorReturn(false),
      sivaItemsESDeletion.onErrorReturn(false)).doOnNext(results -> {
      boolean esSuccess = results.getT1() || results.getT2();
      if (!esSuccess) {
        log.warn("ES deletions failed for productSku: {}", productSku);
      } else {
        log.info("Successfully completed ES deletions for productSku: {}", productSku);
      }
    }).then();

  }


  public void deleteRawDataSources(String productSku) {
    try {
      productRepository.deleteById(productSku);
      log.info("Successfully deleted Product from raw data sources for productSku: {}", productSku);
    } catch (Exception e) {
      log.error("Error deleting raw data sources for productSku: {}", productSku, e);
      throw e;
    }
  }

  void deleteMasterData(TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel) {
    String productSku = terminatedSellerDeletionEventModel.getProductSku();
    String productCode = terminatedSellerDeletionEventModel.getProductCode();
    try {
      if (terminatedSellerDeletionEventModel.isSharedProduct()) {
        // For shared products, only delete items, not master data
        itemRepository.deleteByProductSku(productSku);
        log.info("Deleted items for shared product, productSku: {}", productSku);
      } else {
        // For non-shared products, delete sequentially to avoid connection pool pressure
        try {
          masterDataProductRepository.deleteByProductCode(productCode);
          log.info("Deleted master product data for productCode: {}", productCode);
        } catch (Exception e) {
          log.error("Failed to delete master product data for productCode: {}", productCode, e);
        }

        try {
          masterDataItemRepository.deleteByProductCode(productCode);
          log.info("Deleted master item data for productCode: {}", productCode);
        } catch (Exception e) {
          log.error("Failed to delete master item data for productCode: {}", productCode, e);
        }

        log.info("Completed master data deletion for productSku: {}, productCode: {}", productSku, productCode);
      }
    } catch (Exception e) {
      log.error("Error in master data deletion for productSku: {}, productCode: {}", productSku, productCode, e);
      throw e;
    }
  }

  void performFallbackDeletion(String productSku) {
    try {
      try {
        sivaProductRepository.deleteById(productSku);
        log.info("Fallback product deletion completed for productSku: {}", productSku);
      } catch (Exception e) {
        log.error("Fallback product deletion failed for productSku: {}", productSku, e);
      }
      try {
        sivaItemRepository.deleteByProductSku(productSku);
        log.info("Fallback item deletion completed for productSku: {}", productSku);
      } catch (Exception e) {
        log.error("Fallback item deletion failed for productSku: {}", productSku, e);
      }
    } catch (Exception e) {
      log.error("Critical error in fallback deletion for productSku: {}", productSku, e);
    }
  }
}
