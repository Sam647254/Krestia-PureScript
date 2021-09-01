import express from "express";
import { test } from "./backend";

const app = express();

app.get("/", (request, response) => {
   response.send(test());
});

const port = process.env["PORT"] || 7000;

app.listen(port, () => {
   console.log(`Server started at ${port}`);
});