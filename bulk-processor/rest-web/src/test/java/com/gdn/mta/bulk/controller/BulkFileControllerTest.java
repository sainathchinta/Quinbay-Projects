package com.gdn.mta.bulk.controller;


import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import java.io.IOException;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class BulkFileControllerTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "web";
  private static final String CLIENT_ID = "mta";
  private static final String REQUEST_ID = "request-id";


  @InjectMocks
  BulkFileController controller;

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @BeforeEach
  public void setup() throws IOException {
    initMocks(this);
  }

  @AfterEach
  public void breakdown() {
    verifyNoMoreInteractions(bulkFailedProductFileService);
  }

  @Test
  public void testDeleteFailedProduct() throws ApplicationException, IOException {
    when(bulkFailedProductFileService.deleteFileWithModifiedDateLastWeek()).thenReturn(1);
    GdnRestSimpleResponse<Integer> result =
        controller.deleteFailedProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
    Assertions.assertTrue(result.isSuccess());
    Assertions.assertEquals(1, (int) result.getValue());
    verify(bulkFailedProductFileService).deleteFileWithModifiedDateLastWeek();
  }

  @Test
  public void testDeleteFailedProduct_WithException() throws ApplicationException, IOException {
    when(bulkFailedProductFileService.deleteFileWithModifiedDateLastWeek()).thenThrow(new IOException());
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> controller.deleteFailedProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID));
    } catch (Exception e) {} finally {
      verify(bulkFailedProductFileService).deleteFileWithModifiedDateLastWeek();
    }

  }
}
