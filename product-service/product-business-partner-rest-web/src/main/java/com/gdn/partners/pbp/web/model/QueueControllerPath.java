package com.gdn.partners.pbp.web.model;

public interface QueueControllerPath {
  public static final String BASE_PATH = "/api/queues/";
  public static final String CONSUMERS = "/consumers";
  public static final String STOP = "/stop";
  public static final String START = "/start";
  public static final String STOP_EVENT_STORE_CONSUMERS = CONSUMERS + "/event-stores" + STOP;
  public static final String START_EVENT_STORE_CONSUMERS = CONSUMERS + "/event-stores"
      + START;
  public static final String STOP_CONSUMER_BY_ID = CONSUMERS + "/{id}" + STOP;
  public static final String START_CONSUMER_BY_ID = CONSUMERS + "/{id}" + START;
}
