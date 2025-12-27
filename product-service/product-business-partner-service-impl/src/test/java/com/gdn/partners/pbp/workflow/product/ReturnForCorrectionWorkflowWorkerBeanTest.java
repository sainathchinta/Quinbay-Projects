package com.gdn.partners.pbp.workflow.product;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;

public class ReturnForCorrectionWorkflowWorkerBeanTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PROCESS_CODE = "PROCESS_CODE";
  private static final String NOTES = "notes hahe";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String VENDOR_ERROR_NOTES = "correctionReason";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");

  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;

  @InjectMocks
  ReturnForCorrectionWorkflowWorkerBean workflowWorker;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductMailEventService productMailEventService;

  private Map<String, Object> datas;
  private NeedRevisionNotes needRevisionNotesWebRequest;

  @BeforeEach
  public void initializeTest() throws Exception {
    initMocks(this);

    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setProductCodes(Arrays.asList(PRODUCT_CODE));
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setVendorErrorFields(ERROR_FIELDS);
    screeningProductBulkActionsRequest.setVendorNotes(Arrays.asList(VENDOR_ERROR_NOTES));
    screeningProductBulkActionsRequest.setImageReason(Arrays.asList(VENDOR_ERROR_NOTES));
    screeningProductBulkActionsRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(ADDITIONAL_NOTES);
    needRevisionNotesWebRequest = new NeedRevisionNotes();
    needRevisionNotesWebRequest.setAllVariants(true);
    this.datas = new HashMap<>();
    this.datas.put("productCode", PRODUCT_CODE);
    this.datas.put("processCode", PROCESS_CODE);
    this.datas.put("notes", NOTES);
    this.datas.put("needRevisionNotes", needRevisionNotesWebRequest);

    Mockito.doNothing().when(this.productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, NOTES);
    Mockito.doNothing().when(this.productLevel1WipService)
        .returnDraftForCorrection(PRODUCT_CODE, needRevisionNotesWebRequest, false, false, true);
    Mockito.doNothing().when(this.productLevel1HistoryService).create(PRODUCT_CODE, PROCESS_CODE, NOTES);
    Mockito.doNothing().when(this.productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, NOTES);
  }

  @Test
  public void processTest() throws Exception {
    this.workflowWorker.process(this.datas);
    verify(this.productLevel1WipService).returnDraftForCorrection(PRODUCT_CODE, needRevisionNotesWebRequest, false,
        false, true);
    verify(this.productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, NOTES);
    verify(this.productLevel1HistoryService)
        .createForNeedRevision(PRODUCT_CODE, PROCESS_CODE, NOTES, needRevisionNotesWebRequest, StringUtils.EMPTY);
    verify(this.productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, NOTES);
  }

  @Test
  public void processAutoNeedRevisionTest() throws Exception {
    datas.put("AutoNeedRevision", true);
    datas.put(Constants.VALIDATE_DRAFT_STATE, true);
    this.workflowWorker.process(this.datas);
    verify(this.productLevel1WipService).returnDraftForCorrection(PRODUCT_CODE, needRevisionNotesWebRequest, true,
        false, true);
    verify(this.productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, NOTES);
    verify(this.productLevel1HistoryService).createForNeedRevision(PRODUCT_CODE, PROCESS_CODE, NOTES,
        needRevisionNotesWebRequest, Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION);
    verify(this.productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, NOTES);
  }

  @Test
  public void processScreeningActionTest() throws Exception {
    datas.put(Constants.SCREENING_ACTION, true);
    this.workflowWorker.process(this.datas);
    verify(this.productLevel1WipService).returnDraftForCorrection(PRODUCT_CODE, needRevisionNotesWebRequest, false,
        true, true);
    verify(this.productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, NOTES);
    verify(this.productLevel1HistoryService)
        .createForNeedRevision(PRODUCT_CODE, PROCESS_CODE, NOTES, needRevisionNotesWebRequest, StringUtils.EMPTY);
    verify(this.productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, NOTES);
  }

  @AfterEach
  public void finalizeTest() {
    verifyNoMoreInteractions(this.productLevel3WipService);
    verifyNoMoreInteractions(this.productLevel1WipService);
    verifyNoMoreInteractions(this.productLevel1HistoryService);
    verifyNoMoreInteractions(this.productMailEventService);
  }

}
