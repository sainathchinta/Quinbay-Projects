package com.gdn.mta.bulk.dto;

/**
 * Created by virajjasani on 28/09/16.
 */
public class BulkDownloadMailRecipient {

  private String emailCc;
  private String emailTo;

  public BulkDownloadMailRecipient() {
  }

  public BulkDownloadMailRecipient(String emailCc, String emailTo) {
    this.emailCc = emailCc;
    this.emailTo = emailTo;
  }

  public String getEmailCc() {
    return emailCc;
  }

  public void setEmailCc(String emailCc) {
    this.emailCc = emailCc;
  }

  public String getEmailTo() {
    return emailTo;
  }

  public void setEmailTo(String emailTo) {
    this.emailTo = emailTo;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (!(o instanceof BulkDownloadMailRecipient))
      return false;

    BulkDownloadMailRecipient that = (BulkDownloadMailRecipient) o;

    if (getEmailCc() != null ? !getEmailCc().equals(that.getEmailCc()) : that.getEmailCc() != null)
      return false;
    return getEmailTo() != null ?
        getEmailTo().equals(that.getEmailTo()) :
        that.getEmailTo() == null;

  }

  @Override public int hashCode() {
    int result = getEmailCc() != null ? getEmailCc().hashCode() : 0;
    result = 31 * result + (getEmailTo() != null ? getEmailTo().hashCode() : 0);
    return result;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkDownloadMailRecipient{");
    sb.append("emailCc='").append(emailCc).append('\'');
    sb.append(", emailTo='").append(emailTo).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
