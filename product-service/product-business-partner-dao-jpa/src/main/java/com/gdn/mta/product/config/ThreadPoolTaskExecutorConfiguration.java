package com.gdn.mta.product.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import lombok.Data;

@Configuration
@Data
public class ThreadPoolTaskExecutorConfiguration {

  @Value("${application.auto_batch_update_item_view_config_task.core_pool_size}")
  private int autoBatchUpdateItemViewConfigCorePoolSize;

  @Value("${application.auto_batch_update_item_view_config_task.max_pool_size}")
  private int autoBatchUpdateItemViewConfigMaxPoolSize;

  @Value("${application.auto_batch_update_item_view_config_task.queue_capacity}")
  private int autoBatchUpdateItemViewConfigQueueCapacity;

  @Value("${application.auto_batch_update_item_view_config_task.wait_for_task_complete}")
  private boolean autoBatchUpdateItemViewConfigWaitForTaskComplete;

  @Value("${application.auto_batch_update_item_view_config_task.thread_name_prefix}")
  private String autoBatchUpdateItemViewConfigThreadNamePrefix;

  @Value("${application.send_mail_and_notification_stock_alert_task.thread_name_prefix}")
  private String sendMailAndNotificationStockAlertThreadNamePrefix;

  @Value("${application.merchant_product_mv_indexing_task.core_pool_size}")
  private int merchantProductMVIndexingCorePoolSize;

  @Value("${application.merchant_product_mv_indexing_task.max_pool_size}")
  private int merchantProductMVIndexingMaxPoolSize;

  @Value("${application.merchant_product_mv_indexing_task.queue_capacity}")
  private int merchantProductMVIndexingQueueCapacity;

  @Value("${application.merchant_product_mv_indexing_task.wait_for_task_complete}")
  private boolean merchantProductMVIndexingWaitForTaskComplete;

  @Value("${application.merchant_product_mv_indexing_task.thread_name_prefix}")
  private String merchantProductMVIndexingThreadNamePrefix;

  @Value("${application.product-level3-aggregator.partial-indexer.core-pool-size}")
  private int productLevel3AggregatorPartialIndexerCorePoolSize;

  @Value("${application.product-level3-aggregator.partial-indexer.max-pool-size}")
  private int productLevel3AggregatorPartialIndexerMaxPoolSize;

  @Value("${application.product-level3-aggregator.partial-indexer.queue-capacity}")
  private int productLevel3AggregatorPartialIndexerQueueCapacity;

  @Value("${application.auto_reject_product_wip_need_correction.core_pool_size}")
  private int autoRejectProductWipNeedCorrectionCorePoolSize;

  @Value("${application.auto_reject_product_wip_need_correction.max_pool_size}")
  private int autoRejectProductWipNeedCorrectionMaxPoolSize;

  @Value("${application.auto_reject_product_wip_need_correction.queue_capacity}")
  private int autoRejectProductWipNeedCorrectionQueueCapacity;

  @Value("${application.auto_reject_product_wip_need_correction.wait_for_task_complete}")
  private boolean autoRejectProductWipNeedCorrectionWaitForTaskComplete;

  @Value("${application.auto_reject_product_wip_need_correction.thread_name_prefix}")
  private String autoRejectProductWipNeedCorrectionThreadNamePrefix;

  @Bean
  public ThreadPoolTaskExecutor autoBatchUpdateItemViewConfigExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(autoBatchUpdateItemViewConfigCorePoolSize);
    executor.setMaxPoolSize(autoBatchUpdateItemViewConfigMaxPoolSize);
    executor.setQueueCapacity(autoBatchUpdateItemViewConfigQueueCapacity);
    executor.setWaitForTasksToCompleteOnShutdown(autoBatchUpdateItemViewConfigWaitForTaskComplete);
    executor.setThreadNamePrefix(autoBatchUpdateItemViewConfigThreadNamePrefix);
    return executor;
  }

  @Bean
  public ThreadPoolTaskExecutor sendMailAndNotificationStockAlertExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(autoBatchUpdateItemViewConfigCorePoolSize);
    executor.setMaxPoolSize(autoBatchUpdateItemViewConfigMaxPoolSize);
    executor.setQueueCapacity(autoBatchUpdateItemViewConfigQueueCapacity);
    executor.setWaitForTasksToCompleteOnShutdown(autoBatchUpdateItemViewConfigWaitForTaskComplete);
    executor.setThreadNamePrefix(sendMailAndNotificationStockAlertThreadNamePrefix);
    return executor;
  }

  @Bean
  public ThreadPoolTaskExecutor merchantProductMVIndexingExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(merchantProductMVIndexingCorePoolSize);
    executor.setMaxPoolSize(merchantProductMVIndexingMaxPoolSize);
    executor.setQueueCapacity(merchantProductMVIndexingQueueCapacity);
    executor.setWaitForTasksToCompleteOnShutdown(merchantProductMVIndexingWaitForTaskComplete);
    executor.setThreadNamePrefix(merchantProductMVIndexingThreadNamePrefix);
    return executor;
  }

  @Bean
  public ThreadPoolTaskExecutor productLevel3AggregatorPartialIndexerTaskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(productLevel3AggregatorPartialIndexerCorePoolSize);
    executor.setMaxPoolSize(productLevel3AggregatorPartialIndexerMaxPoolSize);
    executor.setQueueCapacity(productLevel3AggregatorPartialIndexerQueueCapacity);
    return executor;
  }

  @Bean
  public ThreadPoolTaskExecutor autoRejectProductWipNeedCorrectionExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(autoRejectProductWipNeedCorrectionCorePoolSize);
    executor.setMaxPoolSize(autoRejectProductWipNeedCorrectionMaxPoolSize);
    executor.setQueueCapacity(autoRejectProductWipNeedCorrectionQueueCapacity);
    executor.setWaitForTasksToCompleteOnShutdown(autoRejectProductWipNeedCorrectionWaitForTaskComplete);
    executor.setThreadNamePrefix(autoRejectProductWipNeedCorrectionThreadNamePrefix);
    return executor;
  }
}
