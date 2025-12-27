package com.gdn.x.product.service.event.listener;

import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.service.api.ArchiveAndDeleteRejectedMerchantProductDataService;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;

;

public class DeleteRejectedMerchantProductEventListenerTest {

  @InjectMocks
  private DeleteRejectedMerchantProductEventListener deleteRejectedMerchantProductEventListener;

  @Mock
  private ArchiveAndDeleteRejectedMerchantProductDataService archiveAndDeleteRejectedMerchantProductDataService;

  @Mock
  private ObjectMapper objectMapper;

  private static final String MESSAGE = "message";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @Test
  public void deleteRejectedMerchantProductEventListenerTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductCodeDomainEventModel.class))
        .thenReturn(new ProductCodeDomainEventModel());
    deleteRejectedMerchantProductEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(archiveAndDeleteRejectedMerchantProductDataService)
        .archiveAndDeleteRejectedMerchantProductData(Mockito.any());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductCodeDomainEventModel.class);
  }

  @Test
  public void deleteRejectedMerchantProductEventListenerExceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductCodeDomainEventModel.class))
        .thenReturn(new ProductCodeDomainEventModel());
    Mockito.doThrow(new Exception()).when(archiveAndDeleteRejectedMerchantProductDataService)
        .archiveAndDeleteRejectedMerchantProductData(Mockito.any());
    deleteRejectedMerchantProductEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(archiveAndDeleteRejectedMerchantProductDataService)
        .archiveAndDeleteRejectedMerchantProductData(Mockito.any());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductCodeDomainEventModel.class);
  }
}
