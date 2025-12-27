package com.gdn.mta.bulk.service;

import java.util.Map;

/**
 * General Processor Service for queue in X-Bulk
 *
 * @author yohanes.p.k.hadinata
 *
 * @param <T> Data type of object to be saved in queue
 * @param <R1> Return type of {@link GeneralProcessorService#preProcess(Object, Map)}
 * @param <R2> Return type of {@link GeneralProcessorService#process(Object)}
 */
public interface GeneralProcessorService<T, R1, R2> {

  /**
   * Save data to queue in x-bulk
   *
   * @param data data to be saved
   * @param args additional arguments
   * @return
   * @throws Exception
   */
  default R1 preProcess(T data, Map<String, String> args) throws Exception {
	return null;
	  
  }

  /**
   * Listen to x-bulk's queue and process
   *
   * @param bulkProcessQueue
   * @return
   * @throws Exception
   */
  R2 process(T bulkProcessQueue) throws Exception;

}
