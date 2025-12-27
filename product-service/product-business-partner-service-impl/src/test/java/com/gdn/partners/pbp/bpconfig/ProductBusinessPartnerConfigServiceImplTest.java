package com.gdn.partners.pbp.bpconfig;

import java.util.Calendar;
import java.util.Date;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartnerConfig;
import com.gdn.mta.product.repository.ProductBusinessPartnerConfigRepository;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;

/**
 * Created by Vishal on 19/05/18.
 */
public class ProductBusinessPartnerConfigServiceImplTest {

  private static final String DEFAULT_BP_CODE = "bp-code";
  private static final String DEFAULT_STORE_ID = "storeId";
  private static final String DEFAULT_USERNAME = "username";
  @InjectMocks
  private ProductBusinessPartnerConfigServiceImpl businessPartnerConfigService;

  @Mock
  private ProductBusinessPartnerConfigRepository repository;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils
        .setField(businessPartnerConfigService, "defaultMailOptionReactivationHours", 48);
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
  public void notifyMailVisibilityOptionForProductWip_withBeforeReactivationHours()
      throws Exception {
    ProductBusinessPartnerConfig config =
        new ProductBusinessPartnerConfig(DEFAULT_BP_CODE, Calendar.getInstance().getTime());
    Mockito.when(productLevel3WipService
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class))).thenReturn(2);
    Mockito.when(repository
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE)).thenReturn(config);
    boolean result = businessPartnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipService)
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class));
    Mockito.verify(repository)
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE);
    Assertions.assertFalse(result);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWip_withAfterReactivationHours()
      throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.HOUR_OF_DAY, -49);
    ProductBusinessPartnerConfig config =
        new ProductBusinessPartnerConfig(DEFAULT_BP_CODE, calendar.getTime());
    Mockito.when(productLevel3WipService
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class))).thenReturn(2);
    Mockito.when(repository
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE)).thenReturn(config);
    boolean result = businessPartnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipService)
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class));
    Mockito.verify(repository)
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE);
    Assertions.assertTrue(result);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWip_withNoRows() throws Exception {
    Mockito.when(productLevel3WipService
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class))).thenReturn(2);
    Mockito.when(repository
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE)).thenReturn(null);
    boolean result = businessPartnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipService)
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class));
    Mockito.verify(repository)
        .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BP_CODE);
    Assertions.assertTrue(result);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWip_withCountZero() throws Exception {
    Mockito.when(productLevel3WipService
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class))).thenReturn(0);
    boolean result = businessPartnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipService)
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class));
    Assertions.assertFalse(result);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWip_withThrowsException() throws Exception {
    Mockito.when(productLevel3WipService
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class)))
        .thenThrow(new ApplicationRuntimeException());
    boolean result = businessPartnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipService)
        .findCountByExceedingActivationDate(Mockito.eq(DEFAULT_STORE_ID),
            Mockito.eq(DEFAULT_BP_CODE), Mockito.any(Date.class));
    Assertions.assertFalse(result);
  }

  @Test
  public void saveTest_success() throws Exception {
    ProductBusinessPartnerConfig config = new ProductBusinessPartnerConfig();
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(DEFAULT_BP_CODE, Calendar.getInstance().getTime());
    Mockito.when(repository.save(Mockito.any(ProductBusinessPartnerConfig.class)))
        .thenReturn(config);
    businessPartnerConfigService.save(DEFAULT_STORE_ID, DEFAULT_USERNAME, request);
    Mockito.verify(repository).save(Mockito.any(ProductBusinessPartnerConfig.class));
  }

  @Test
  public void saveTest_failed() throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(DEFAULT_BP_CODE, Calendar.getInstance().getTime());
    Mockito.when(repository.save(Mockito.any(ProductBusinessPartnerConfig.class)))
        .thenThrow(new ApplicationRuntimeException());
    try {
      businessPartnerConfigService.save(DEFAULT_STORE_ID, DEFAULT_USERNAME, request);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(repository).save(Mockito.any(ProductBusinessPartnerConfig.class));
    }
  }

}
