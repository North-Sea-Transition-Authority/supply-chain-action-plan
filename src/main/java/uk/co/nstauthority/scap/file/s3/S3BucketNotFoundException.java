package uk.co.nstauthority.scap.file.s3;

public class S3BucketNotFoundException extends RuntimeException {
  public S3BucketNotFoundException(String bucketName) {
    super("Could not find bucket %s".formatted(bucketName));
  }
}
