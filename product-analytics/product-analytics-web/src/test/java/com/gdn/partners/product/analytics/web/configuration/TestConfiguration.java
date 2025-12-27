package com.gdn.partners.product.analytics.web.configuration;

import com.gdn.partners.product.analytics.service.AutoApprovedService;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import com.gdn.partners.product.analytics.service.SellerAnalyticsService;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.product.analytics.service.AutoQCDetailService;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.SellerDataWrapperService;
import com.gdn.partners.product.analytics.service.SellerDetailService;
import com.gdn.partners.product.analytics.service.UserFeedbackService;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import com.gdn.partners.product.analytics.service.bigQuery.DownloadSellerInfoBigQueryService;

@Configuration
public class TestConfiguration {

  @Bean
  public DownloadSellerInfoBigQueryService downloadSellerInfoBigQueryService() {
    return Mockito.mock(DownloadSellerInfoBigQueryService.class);
  }

  @Bean
  public AutoQCDetailService autoQCDetailService() {
    return Mockito.mock(AutoQCDetailService.class);
  }

  @Bean
  public SellerDetailService sellerDetailService() {
    return Mockito.mock(SellerDetailService.class);
  }

  @Bean
  public SellerDataWrapperService sellerDataWrapperService() {
    return Mockito.mock(SellerDataWrapperService.class);
  }

  @Bean
  public KafkaProducerService kafkaProducerService() {
    return Mockito.mock(KafkaProducerService.class);
  }

  @Bean
  public AutoApprovedService autoApprovedService() {
    return Mockito.mock(AutoApprovedService.class);
  }

  @Bean
  public UserFeedbackService userFeedbackService() {
    return Mockito.mock(UserFeedbackService.class);
  }

  @Bean
  public TerminatedSellerDeletionService terminatedSellerDeletionService() {
    return Mockito.mock(TerminatedSellerDeletionService.class);
  }

  @Bean
  public ImageDeleteService imageDeleteService() {
    return Mockito.mock(ImageDeleteService.class);
  }

  @Bean
  public SellerAnalyticsService sellerAnalyticsService() {
    return Mockito.mock(SellerAnalyticsService.class);
  }

  @Bean
  public ProductOptimisationService productOptimisationService() {
    return Mockito.mock(ProductOptimisationService.class);
  }

  @Bean
  public ProductAttributeExtractionsService productAttributeExtractionsService() {
    return Mockito.mock(ProductAttributeExtractionsService.class);
  }
}
