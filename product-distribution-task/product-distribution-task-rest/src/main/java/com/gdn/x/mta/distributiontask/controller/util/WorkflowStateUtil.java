package com.gdn.x.mta.distributiontask.controller.util;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.google.common.collect.ImmutableSet;

import java.util.Set;

/**
 * Created by virajjasani on 27/11/16.
 */
public class WorkflowStateUtil {

  public static final Set<WorkflowState> VENDOR_PRODUCT_APPROVED_STATES = ImmutableSet
      .of(WorkflowState.PASSED);
}
