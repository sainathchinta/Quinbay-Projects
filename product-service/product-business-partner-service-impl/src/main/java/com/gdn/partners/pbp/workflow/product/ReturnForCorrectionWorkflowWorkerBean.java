package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ReturnForCorrectionWorkflowWorkerBean.BEAN_NAME
    + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ReturnForCorrectionWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "returnForCorrection";
  
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String NOTES = "notes";
  private static final String NEED_REVISION_NOTES = "needRevisionNotes";

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    String notes = String.valueOf(datas.get(NOTES));
    String username = Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION;
    boolean autoNeedRevision = false;
    boolean screeningAction = false;
    boolean validateDraftState = true;
    if(datas.containsKey(Constants.AUTO_NEED_REVISION_FLAG)) {
      autoNeedRevision = Boolean.valueOf(String.valueOf(datas.get(Constants.AUTO_NEED_REVISION_FLAG)));
    }
    if(datas.containsKey(Constants.SCREENING_ACTION)) {
      screeningAction = Boolean.valueOf(String.valueOf(datas.get(Constants.SCREENING_ACTION)));
    }
    if(datas.containsKey(Constants.VALIDATE_DRAFT_STATE)) {
      validateDraftState = Boolean.valueOf(String.valueOf(datas.get(Constants.VALIDATE_DRAFT_STATE)));
    }
    NeedRevisionNotes revisionNotes = (NeedRevisionNotes) datas.get(NEED_REVISION_NOTES);
    Map<String, Object> notificationDatas = new HashMap<String, Object>();
    notificationDatas.put(PROCESS_CODE, WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue());
    notificationDatas.put(PRODUCT_CODE, productCode);
    notificationDatas.put(NOTES, notes);
    this.productLevel1WipService
        .returnDraftForCorrection(productCode, revisionNotes, autoNeedRevision, screeningAction, validateDraftState);
    this.productLevel3WipService.returnDraftForCorrection(productCode, notes);
    this.productLevel1HistoryService.createForNeedRevision(productCode, processCode, notes, revisionNotes,
        (autoNeedRevision) ? username : StringUtils.EMPTY);
    this.productMailEventService.sendDomainEventForSentForCorrection(productCode, notes);
  }
}
