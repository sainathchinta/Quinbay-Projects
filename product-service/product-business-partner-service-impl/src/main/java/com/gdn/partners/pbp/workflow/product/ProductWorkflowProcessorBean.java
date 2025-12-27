package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.entity.workflow.product.ProductWfHistory;
import com.gdn.partners.pbp.repository.workflow.ProductWfHistoryRepository;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.partners.pbp.workflow.WorkflowConfiguration;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ProductWorkflowProcessorBean.BEAN_NAME + WorkflowProcessor.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ProductWorkflowProcessorBean implements WorkflowProcessor {

  public static final String BEAN_NAME = "product";
  
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductWorkflowProcessorBean.class);
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String IN_VENDOR = "IN_VENDOR";
  private static final String DRAFT = "DRAFT";

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private ProductWfRepository productWorkflowRepository;

  @Autowired
  private ProductWfHistoryRepository productWorkflowHistoryRepository;

  @Value("${populate.product.workflow.history}")
  private boolean populateWorkFlowHistory;

  @Autowired
  public ProductWorkflowProcessorBean(ObjectMapper objectMapper,
      @Value("${workflow.product.configuration}") String configuration,
      @Value("${workflow.product.worker}") String worker) throws Exception {
    Map<String, WorkflowConfiguration> configurations =
        objectMapper.readValue(configuration, new TypeReference<Map<String, WorkflowConfiguration>>() {});
    Map<String, String> workers = objectMapper.readValue(worker, new TypeReference<Map<String, String>>() {});
    for (Entry<String, WorkflowConfiguration> entry : configurations.entrySet()) {
      ProductWorkflowProcessorBean.CONFIGURATIONS.put(entry.getKey(), entry.getValue());
    }
    for (Entry<String, String> entry : workers.entrySet()) {
      ProductWorkflowProcessorBean.WORKERS.put(entry.getKey(), entry.getValue());
    }
  }

  @Override
  public void validate(List<String> states, Map<String, Object> variables) {
    if (!states.isEmpty()) {
      String storeId = String.valueOf(variables.get(Constants.STORE_ID));
      String productCode = String.valueOf(variables.get(PRODUCT_CODE));
      if (!states.contains("ANY")) {
        for (String state : states) {
          ProductWf productWorkflow =
              this.productWorkflowRepository
                  .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(storeId,
                  productCode, state);
          if (productWorkflow == null) {
            throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
                "invalid workflow state: Expected State= " + state);
          }
        }
      }
    }
  }

  @Override
  public void complete(List<String> states, Map<String, Object> variables) {
    if (!states.isEmpty()) {
      String storeId = String.valueOf(variables.get(Constants.STORE_ID));
      String productCode = String.valueOf(variables.get(PRODUCT_CODE));
      if (states.contains("ANY")) {
        List<ProductWf> productWorkflows =
            this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
        this.productWorkflowRepository.deleteAll(productWorkflows);
        LOGGER.info(LoggerStandard.getProductWorkflowLog("ANY", storeId, productCode));
      } else {
        for (String state : states) {
          ProductWf productWorkflow =
              this.productWorkflowRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(storeId,
                  productCode, state);
          this.productWorkflowRepository.delete(productWorkflow);
          LOGGER.info(LoggerStandard.getProductWorkflowLog(state, storeId, productCode));
        }
      }
    }
  }

  @Override
  public void generate(List<String> states, Map<String, Object> variables) {
    if (!states.isEmpty()) {
      String storeId = String.valueOf(variables.get(Constants.STORE_ID));
      String productCode = String.valueOf(variables.get(PRODUCT_CODE));
      for (String state : states) {
        ProductWf productWorkflow = new ProductWf();
        productWorkflow.setStoreId(storeId);
        productWorkflow.setProductCode(productCode);
        productWorkflow.setState(state);
        ProductWfHistory productWorkflowHistory = new ProductWfHistory();
        productWorkflowHistory.setStoreId(storeId);
        productWorkflowHistory.setProductCode(productCode);
        productWorkflowHistory.setState(state);
        this.productWorkflowRepository.save(productWorkflow);
        this.productWorkflowHistoryRepository.save(productWorkflowHistory);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void process(String processCode, Map<String, Object> datas) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      LOGGER.warn("StoreId is empty, in ProductWorkFlowProcessorBean for productCode : {}",
          String.valueOf(datas.get(PRODUCT_CODE)));
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    WorkflowConfiguration workflowConfiguration = ProductWorkflowProcessorBean.CONFIGURATIONS.get(processCode);
    String beanName = ProductWorkflowProcessorBean.WORKERS.get(processCode);
    Map<String, Object> variables = new HashMap<>();
    variables.put(Constants.STORE_ID, storeId);
    variables.put(PRODUCT_CODE, productCode);
    datas.put(PROCESS_CODE, processCode);
    datas.put(Constants.STORE_ID, storeId);
    boolean validateDraftState = true;
    if(datas.containsKey(Constants.VALIDATE_DRAFT_STATE)) {
      validateDraftState = Boolean.valueOf(String.valueOf(datas.get(Constants.VALIDATE_DRAFT_STATE)));
    }
    ProductWf productWorkflow = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(storeId, productCode, IN_VENDOR);
    boolean returnForCorrection = isReturnForCorrection(datas, productWorkflow);
    if (validateDraftState) {
      if (returnForCorrection && populateWorkFlowHistory) {
        ProductWfHistory productWfHistory =
            this.productWorkflowHistoryRepository.findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(storeId,
                productCode, DRAFT);
        if (Objects.isNull(productWfHistory)) {
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
              "invalid workflow state: Expected State is IN_VENDOR");
        }
      } else if (!returnForCorrection) {
        this.validate(workflowConfiguration.getCurrentStates(), variables);
      }
    }
    WorkflowWorker workflowWorker =
        (WorkflowWorker) this.autowireCapableBeanFactory.getBean(beanName + WorkflowWorker.SUFFIX_BEAN_NAME);
    workflowWorker.process(datas);

    if (isReturnForCorrection(datas, productWorkflow)) {
      ProductWf productWorkflowInVendor = this.productWorkflowRepository
          .findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(storeId, productCode, IN_VENDOR);
      this.productWorkflowRepository.delete(productWorkflowInVendor);
      LOGGER.info(LoggerStandard.getProductWorkflowLog(IN_VENDOR, storeId, productCode));
    } else {
      this.complete(workflowConfiguration.getCurrentStates(), variables);
    }
    this.generate(workflowConfiguration.getNextStates(), variables);
    if (workflowConfiguration.isAutoRun()
        && !StringUtils.isEmpty(workflowConfiguration.getNextProcessCode())) {
      try {
        this.process(workflowConfiguration.getNextProcessCode(), datas);
      } catch (Exception e) {
        ProductWorkflowProcessorBean.LOGGER.error("failed auto run next process", e);
      }
    }
  }

  private static boolean isReturnForCorrection(Map<String, Object> datas, ProductWf productWorkflow) {
    return (WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue().equals(datas.get(PROCESS_CODE))) && Objects.nonNull(
        productWorkflow);
  }

  @Override
  public void deleteProductWfHistoryByStoreIdAndProductCode(String storeId, String productCode){
    productWorkflowHistoryRepository.deleteByStoreIdAndProductCode(storeId, productCode);
  }
}
