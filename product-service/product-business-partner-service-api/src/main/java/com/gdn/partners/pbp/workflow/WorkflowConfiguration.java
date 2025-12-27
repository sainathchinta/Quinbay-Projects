package com.gdn.partners.pbp.workflow;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class WorkflowConfiguration implements Serializable {

  private static final long serialVersionUID = -9090846697553576894L;
  private String nextProcessCode;
  private List<String> currentStates = new ArrayList<>();
  private List<String> nextStates = new ArrayList<>();
  private boolean autoRun = false;

  public WorkflowConfiguration() {}

  public WorkflowConfiguration(String nextProcessCode, List<String> currentStates, List<String> nextStates,
      boolean autoRun) {
    super();
    this.nextProcessCode = nextProcessCode;
    this.currentStates = currentStates;
    this.nextStates = nextStates;
    this.autoRun = autoRun;
  }

  public String getNextProcessCode() {
    return nextProcessCode;
  }

  public void setNextProcessCode(String nextProcessCode) {
    this.nextProcessCode = nextProcessCode;
  }

  public List<String> getCurrentStates() {
    return currentStates;
  }

  public void setCurrentStates(List<String> currentStates) {
    this.currentStates = currentStates;
  }

  public List<String> getNextStates() {
    return nextStates;
  }

  public void setNextStates(List<String> nextStates) {
    this.nextStates = nextStates;
  }

  public boolean isAutoRun() {
    return autoRun;
  }

  public void setAutoRun(boolean autoRun) {
    this.autoRun = autoRun;
  }

  @Override
  public String toString() {
    return String.format("WorkflowConfiguration [nextProcessCode=%s, currentStates=%s, nextStates=%s, autoRun=%s]",
        nextProcessCode, currentStates, nextStates, autoRun);
  }

}
