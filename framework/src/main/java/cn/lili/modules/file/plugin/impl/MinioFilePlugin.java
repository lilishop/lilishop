package cn.lili.modules.file.plugin.impl;

import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.system.entity.dto.OssSetting;
import io.minio.*;
import io.minio.errors.ErrorResponseException;
import io.minio.messages.DeleteObject;
import lombok.extern.slf4j.Slf4j;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;


/**
 * MINIO文件插件
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2022/6/6 17:45
 */
@Slf4j
public class MinioFilePlugin implements FilePlugin {

    private OssSetting ossSetting;

    public MinioFilePlugin(OssSetting ossSetting) {
        this.ossSetting = ossSetting;
    }

    /**
     * 桶占位符
     */
    private static final String BUCKET_PARAM = "${bucket}";
    /**
     * bucket权限-只读
     */
    private static final String READ_ONLY = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucket\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "\"]},{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:GetObject\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "/*\"]}]}";
    /**
     * bucket权限-只读
     */
    private static final String WRITE_ONLY = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucketMultipartUploads\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "\"]},{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:AbortMultipartUpload\",\"s3:DeleteObject\",\"s3:ListMultipartUploadParts\",\"s3:PutObject\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "/*\"]}]}";
    /**
     * bucket权限-读写
     */
    private static final String READ_WRITE = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucket\",\"s3:ListBucketMultipartUploads\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "\"]},{\"Effect\":\"Allow\",\"Principal\":{\"AWS\":[\"*\"]},\"Action\":[\"s3:DeleteObject\",\"s3:GetObject\",\"s3:ListMultipartUploadParts\",\"s3:PutObject\",\"s3:AbortMultipartUpload\"],\"Resource\":[\"arn:aws:s3:::" + BUCKET_PARAM + "/*\"]}]}";


    private MinioClient minioClient;


    @Override
    public OssEnum pluginName() {
        return OssEnum.MINIO;
    }

    @Override
    public String pathUpload(String filePath, String key) {
        try {
            return this.inputStreamUpload(new FileInputStream(filePath), key);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public String inputStreamUpload(InputStream inputStream, String key) {
        String bucket = "";
        try {
            MinioClient client = getOssClient();
            bucket = ossSetting.getM_bucketName();
            PutObjectArgs putObjectArgs = PutObjectArgs.builder()
                    .bucket(bucket).stream(inputStream, inputStream.available(), 5 * 1024 * 1024)
                    .object(key)
                    .contentType("image/png")
                    .build();
            client.putObject(putObjectArgs);
        } catch (ErrorResponseException e) {
            e.printStackTrace();
            return null;
        } catch (Exception e) {
            log.error("上传失败2，", e);
            return null;
        }
        //拼接出可访问的url地址
        return ossSetting.getM_endpoint() + "/" + bucket + "/" + key;
    }


    @Override
    public void deleteFile(List<String> key) {
        if (key == null || key.isEmpty()) {
            return;
        }
        MinioClient ossClient = getOssClient();
        List<DeleteObject> objectList = key.stream().map(DeleteObject::new).collect(Collectors.toList());
        ossClient.removeObjects(RemoveObjectsArgs.builder().objects(objectList).build());
    }


    /**
     * 获取oss client
     *
     * @return
     */
    private MinioClient getOssClient() {
        if (minioClient != null) {
            return this.minioClient;
        }
        synchronized (this) {
            if (minioClient == null) {
                //创建客户端
                this.minioClient = MinioClient.builder()
                        .endpoint(ossSetting.getM_endpoint())
                        .credentials(ossSetting.getM_accessKey(), ossSetting.getM_secretKey())
                        .build();
                try {
                    //查看对应的bucket是否已经存在，不存在则创建
                    if (!minioClient.bucketExists(BucketExistsArgs.builder().bucket(ossSetting.getM_bucketName()).build())) {
                        //创建bucket
                        MakeBucketArgs makeBucketArgs = MakeBucketArgs.builder().bucket(ossSetting.getM_bucketName()).build();
                        this.minioClient.makeBucket(makeBucketArgs);
                        setBucketPolicy(this.minioClient, ossSetting.getM_bucketName(), "read-write");
                        log.info("创建minio桶成功{}", ossSetting.getM_bucketName());
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("创建[{}]bucket失败", ossSetting.getM_bucketName());
                }
            }
        }
        return minioClient;
    }


    /**
     * 更新桶权限策略
     *
     * @param bucket 桶
     * @param policy 权限
     */
    public static void setBucketPolicy(MinioClient client, String bucket, String policy) throws Exception {
        switch (policy) {
            case "read-only":
                client.setBucketPolicy(SetBucketPolicyArgs.builder().bucket(bucket).config(READ_ONLY.replace(BUCKET_PARAM, bucket)).build());
                break;
            case "write-only":
                client.setBucketPolicy(SetBucketPolicyArgs.builder().bucket(bucket).config(WRITE_ONLY.replace(BUCKET_PARAM, bucket)).build());
                break;
            case "read-write":
                client.setBucketPolicy(SetBucketPolicyArgs.builder().bucket(bucket).region("public").config(READ_WRITE.replace(BUCKET_PARAM, bucket)).build());
                break;
            case "none":
            default:
                break;
        }
    }


}
